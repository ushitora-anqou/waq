module Channel = struct
  type recv_error = [ `Closed ]
  type 'a recv_result = ('a, recv_error) result

  type 'a t = {
    mutex : Mutex.t;
    mutable closed : bool;
    queued_items : 'a Queue.t;
    pending_recv :
      (bool Atomic.t (* canceled? *) * ('a recv_result -> Scheduler.Task.t))
      Queue.t;
    pending_send :
      (bool Atomic.t (* canceled? *) * ('a * Scheduler.Task.t)) Queue.t;
    bound_size : int;
  }

  let make size =
    assert (size >= 0);
    {
      mutex = Mutex.create ();
      closed = false;
      queued_items = Queue.create ();
      pending_recv = Queue.create ();
      pending_send = Queue.create ();
      bound_size = size;
    }

  let close k scheduler ch =
    (* FIXME: make pending_send panic? *)
    Mutex.lock ch.mutex;
    ch.closed <- true;
    let rec loop () =
      match Queue.take_opt ch.pending_recv with
      | None -> ()
      | Some (canceled, gen_task) ->
          if Atomic.compare_and_set canceled false true then
            scheduler |> Scheduler.enqueue_runnable (gen_task (Error `Closed));
          loop ()
    in
    loop ();
    Mutex.unlock ch.mutex;
    Effect.Deep.continue k ()

  let check_invariant ch =
    (*
      S1 #items = 0, #send = 0, #recv = 0 (initial)
      S2 #items = 0, #send = 0, #recv > 0 (from S1)
      S3 #items = 0, #send > 0, #recv = 0 (from S1, size = 0)
        assert (#items = 0 = bound_size)
      -- #items = 0, #send > 0, #recv > 0
      S5 #items > 0, #send = 0, #recv = 0 (from S1)
      -- #items > 0, #send = 0, #recv > 0
      S6 #items > 0, #send > 0, #recv = 0 (from S5)
        assert (#items = bound_size)
      -- #items > 0, #send > 0, #recv > 0
    *)
    match
      ( Queue.length ch.queued_items > 0,
        Queue.length ch.pending_send > 0,
        Queue.length ch.pending_recv > 0 )
    with
    | false, true, true | true, false, true | true, true, true ->
        Util.unreachable ~__FUNCTION__
    | _, true, false when Queue.length ch.queued_items <> ch.bound_size ->
        Util.unreachable ~__FUNCTION__
    | _ -> ()

  module Select = struct
    type ('a, 'b) spec =
      | Send of ('a t * 'a * (unit -> 'b))
      | Recv of ('a t * ('a recv_result -> 'b))

    type ('ty, 'v, 'b) t =
      | [] : ('v, 'v, 'b) t
      | ( :: ) :
          ('a, 'b) spec * ('ty, 'v, 'b) t
          -> (('a, 'b) spec -> 'ty, 'v, 'b) t

    let remove_canceled_tasks tasks =
      let new_q = Queue.create () in
      let rec loop () =
        match Queue.take_opt tasks with
        | None -> Queue.transfer new_q tasks
        | Some (canceled, _) when Atomic.get canceled -> loop ()
        | Some tupl ->
            Queue.add tupl new_q;
            loop ()
      in
      loop ()

    let select_send_spec k task scheduler canceled ch item handler =
      Mutex.lock ch.mutex;
      Fun.protect ~finally:(fun () -> Mutex.unlock ch.mutex) @@ fun () ->
      if ch.closed then
        failwith "Channel.Select.select_send_spec: closed channel"
      else (
        check_invariant ch;
        (*
          S1 #items = 0, #send = 0, #recv = 0 (initial)
            if #items = 0 < bound_size then push to items (go to S5)
            if #items = 0 = bound_size then push to send (go to S3)
          S2 #items = 0, #send = 0, #recv > 0
            pop pending recv and send to it (go to S1 or S2)
          S3 #items = 0, #send > 0, #recv = 0
            assert (#items = 0 = bound_size)
            push to send (go to S3)
          S5 #items > 0, #send = 0, #recv = 0
            if #items < bound_size then push to items (go to S5)
            if #items = bound_size then push to send (go to S6)
          S6 #items > 0, #send > 0, #recv = 0
            assert (#items = bound_size)
            push to send (go to S6)
        *)
        let push_to_pending_send () =
          remove_canceled_tasks ch.pending_send;
          let task =
            task
            |> Scheduler.Task.with_k (fun () ->
                   Effect.Deep.continue k (handler ()))
          in
          ch.pending_send |> Queue.add (canceled, (item, task));
          `Loop
        in
        let push_to_queued_items () =
          Queue.add item ch.queued_items;
          `Continue (handler ())
        in
        let pop_pending_recv_and_send_to_it () =
          let rec resolve_pending_recv () =
            match Queue.take_opt ch.pending_recv with
            | None ->
                (* We failed to pop an effective pending recv,
                   so let's fallback to S1. *)
                if Queue.length ch.queued_items < ch.bound_size then
                  push_to_queued_items ()
                else push_to_pending_send ()
            | Some (canceled, gen_task) -> (
                match Atomic.compare_and_set canceled false true with
                | false -> resolve_pending_recv ()
                | true ->
                    scheduler |> Scheduler.enqueue_runnable (gen_task (Ok item));
                    `Continue (handler ()))
          in
          resolve_pending_recv ()
        in
        let set_canceled f =
          (* Set true to `canceled` atomically to ensure that only one branch
             should be run for each Select. *)
          if Atomic.compare_and_set canceled false true then f ()
          else `Finish (* another branch has already been selected *)
        in
        match
          ( Queue.length ch.queued_items > 0,
            Queue.length ch.pending_send > 0,
            Queue.length ch.pending_recv > 0 )
        with
        | _, false, false when Queue.length ch.queued_items < ch.bound_size ->
            set_canceled push_to_queued_items
        | _, false, false when Queue.length ch.queued_items = ch.bound_size ->
            push_to_pending_send ()
        | _, true, false when Queue.length ch.queued_items = ch.bound_size ->
            push_to_pending_send ()
        | false, false, true -> set_canceled pop_pending_recv_and_send_to_it
        | _ -> Util.unreachable ~__FUNCTION__)

    let select_recv_spec k task scheduler canceled ch handler =
      Mutex.lock ch.mutex;
      Fun.protect ~finally:(fun () -> Mutex.unlock ch.mutex) @@ fun () ->
      if ch.closed then
        if Atomic.compare_and_set canceled false true then
          `Continue (handler (Error `Closed))
        else `Finish
      else (
        check_invariant ch;
        (*
          S1 #items = 0, #send = 0, #recv = 0 (initial)
            push to recv (go to S2)
          S2 #items = 0, #send = 0, #recv > 0
            push to recv (go to S2)
          S3 #items = 0, #send > 0, #recv = 0
            assert (#items = 0 = bound_size)
            pop send and receive from it (go to S1 or S3)
          S5 #items > 0, #send = 0, #recv = 0
            pop from items (go to S1 or S5)
          S6 #items > 0, #send > 0, #recv = 0
            assert (#items = bound_size)
            pop from items and send (go to S5 or S6)
        *)
        let push_to_pending_recv () =
          remove_canceled_tasks ch.pending_recv;
          let task item =
            (* Note that ch.mutex isn't necessarily locked in this function. *)
            task
            |> Scheduler.Task.with_k (fun () ->
                   Effect.Deep.continue k (handler item))
          in
          ch.pending_recv |> Queue.add (canceled, task);
          `Loop
        in
        let pop_from_pending_send ~when_failed ~when_suceeded =
          let rec resolve_pending_send () =
            match Queue.take_opt ch.pending_send with
            | None -> when_failed ()
            | Some (canceled, (item, task)) -> (
                match Atomic.compare_and_set canceled false true with
                | false -> resolve_pending_send ()
                | true ->
                    scheduler |> Scheduler.enqueue_runnable task;
                    when_suceeded item)
          in
          resolve_pending_send ()
        in
        let pop_from_queued_items () =
          let item = Queue.pop ch.queued_items in
          `Continue (handler (Ok item))
        in
        let set_canceled f =
          (* Set true to `canceled` atomically to ensure that only one branch
             should be run for each Select. *)
          if Atomic.compare_and_set canceled false true then f ()
          else `Finish (* another branch has already been selected *)
        in
        match
          ( Queue.length ch.queued_items > 0,
            Queue.length ch.pending_send > 0,
            Queue.length ch.pending_recv > 0 )
        with
        | false, false, _ -> push_to_pending_recv ()
        | false, true, false when ch.bound_size = 0 ->
            set_canceled @@ fun () ->
            pop_from_pending_send
              ~when_suceeded:(fun item -> `Continue (handler (Ok item)))
              ~when_failed:(fun () ->
                (* We failed to pop an effective pending send,
                 so let's fallback to S1. *)
                push_to_pending_recv ())
        | true, false, false -> set_canceled pop_from_queued_items
        | true, true, false when Queue.length ch.queued_items = ch.bound_size ->
            set_canceled @@ fun () ->
            pop_from_pending_send
              ~when_suceeded:(fun item -> ch.queued_items |> Queue.add item)
              ~when_failed:(fun () ->
                (* We failed to pop an effective pending send,
                 so let's fallback to S5 i.e., do nothing here. *)
                ());
            pop_from_queued_items ()
        | _ -> Util.unreachable ~__FUNCTION__)

    let rec loop : type ty v b.
        (b, unit) continuation ->
        Scheduler.Task.t ->
        Scheduler.t ->
        bool Atomic.t ->
        (unit -> b) option ->
        (ty, v, b) t ->
        unit =
     fun k task scheduler canceled default -> function
      | [] when Option.is_some default ->
          if Atomic.compare_and_set canceled false true then
            Effect.Deep.continue k (Option.get default ())
      | [] -> ()
      | Send (ch, item, handler) :: rest -> (
          match select_send_spec k task scheduler canceled ch item handler with
          | `Loop -> loop k task scheduler canceled default rest
          | `Finish -> ()
          | `Continue k_arg -> Effect.Deep.continue k k_arg)
      | Recv (ch, handler) :: rest -> (
          match select_recv_spec k task scheduler canceled ch handler with
          | `Loop -> loop k task scheduler canceled default rest
          | `Finish -> ()
          | `Continue k_arg -> Effect.Deep.continue k k_arg)

    let f : type ty v b.
        (unit -> b) option ->
        (ty, v, b) t ->
        (b, unit) continuation ->
        Scheduler.Task.t ->
        Scheduler.t ->
        unit =
     fun default specs k task scheduler ->
      let canceled = Atomic.make false in
      loop k task scheduler canceled default specs
  end
end

type _ Effect.t +=
  | Yield : unit Effect.t
  | Get_worker_id : int Effect.t
  | Sleep : float -> unit Effect.t
  | Wait_write : Unix.file_descr -> unit Effect.t
  | Wait_read : Unix.file_descr -> unit Effect.t
  | Send_to_channel : ('a * 'a Channel.t) -> unit Effect.t
  | Recv_from_channel : 'a Channel.t -> 'a Channel.recv_result Effect.t
  | Select :
      ((unit -> 'b) option * ('ty, 'v, 'b) Channel.Select.t)
      -> 'b Effect.t
  | Close_channel : 'a Channel.t -> unit Effect.t

module Io_waiter = struct
  type time = float

  type state = {
    mutex : Mutex.t;
    mutable writing_tasks : (Scheduler.Task.t * Unix.file_descr) list;
    mutable reading_tasks : (Scheduler.Task.t * Unix.file_descr) list;
    mutable sleeping_tasks : (Scheduler.Task.t * time) list;
    efd : Eventfd.t;
    mutable terminated : bool;
  }

  type t = { dom : unit Domain.t; state : state }

  let handle_timeout state scheduler =
    let now = Unix.gettimeofday () in

    let all_tasks = state.sleeping_tasks in
    state.sleeping_tasks <- [];
    all_tasks
    |> List.iter (fun (task, until) ->
           if until <= now then Scheduler.enqueue_runnable task scheduler
           else state.sleeping_tasks <- (task, until) :: state.sleeping_tasks);

    let next_timeout =
      state.sleeping_tasks
      |> List.fold_left
           (fun a (_, t) -> Some (match a with None -> t | Some a -> min a t))
           None
      |> Option.fold ~none:(-1.) ~some:(fun x -> x -. now)
    in
    next_timeout

  let handle_reading ready_fds state scheduler =
    let is_ready fd = List.exists (( = ) fd) ready_fds in

    let all_tasks = state.reading_tasks in
    state.reading_tasks <- [];
    all_tasks
    |> List.iter (fun (task, fd) ->
           if is_ready fd then Scheduler.enqueue_runnable task scheduler
           else state.reading_tasks <- (task, fd) :: state.reading_tasks);

    (* Clear eventfd *)
    if is_ready (Eventfd.fd_read_to_poll state.efd) then Eventfd.clear state.efd;

    ()

  let handle_writing ready_fds state scheduler =
    let all_tasks = state.writing_tasks in
    state.writing_tasks <- [];
    all_tasks
    |> List.iter (fun (task, fd) ->
           if List.exists (( = ) fd) ready_fds then
             Scheduler.enqueue_runnable task scheduler
           else state.writing_tasks <- (task, fd) :: state.writing_tasks);
    ()

  let main state scheduler () =
    let rec loop () =
      Mutex.lock state.mutex;
      if state.terminated then Mutex.unlock state.mutex
      else
        let timeout = handle_timeout state scheduler in
        let reading = state.reading_tasks |> List.map snd in
        let writing = state.writing_tasks |> List.map snd in
        Mutex.unlock state.mutex;

        match
          Unix.select
            (Eventfd.fd_read_to_poll state.efd :: reading)
            writing [] timeout
        with
        | ready_reading, ready_writing, _ ->
            Mutex.lock state.mutex;
            handle_reading ready_reading state scheduler;
            handle_writing ready_writing state scheduler;
            Mutex.unlock state.mutex;
            loop ()
    in
    loop ()

  let start scheduler =
    let state =
      {
        mutex = Mutex.create ();
        writing_tasks = [];
        reading_tasks = [];
        sleeping_tasks = [];
        efd = Eventfd.make ();
        terminated = false;
      }
    in
    { dom = Domain.spawn (main state scheduler); state }

  let terminate w =
    Mutex.lock w.state.mutex;
    w.state.terminated <- true;
    Eventfd.signal w.state.efd;
    Mutex.unlock w.state.mutex;
    Domain.join w.dom

  let enqueue_sleeping task until w =
    Mutex.lock w.state.mutex;
    w.state.sleeping_tasks <- (task, until) :: w.state.sleeping_tasks;
    Eventfd.signal w.state.efd;
    Mutex.unlock w.state.mutex

  let enqueue_writing task fd w =
    Mutex.lock w.state.mutex;
    w.state.writing_tasks <- (task, fd) :: w.state.writing_tasks;
    Eventfd.signal w.state.efd;
    Mutex.unlock w.state.mutex

  let enqueue_reading task fd w =
    Mutex.lock w.state.mutex;
    w.state.reading_tasks <- (task, fd) :: w.state.reading_tasks;
    Eventfd.signal w.state.efd;
    Mutex.unlock w.state.mutex
end

module Worker = struct
  type t = {
    id : int;
    mutable dom : unit Domain.t option;
    mutable last_exc : string option;
  }

  let upd_k task k a =
    Scheduler.Task.with_k (fun () -> Effect.Deep.continue k a) task

  let main scheduler io_waiter w () =
    let rec loop () =
      match Scheduler.dequeue_runnable ~worker_id:w.id scheduler with
      | `Terminated -> ()
      | `Ok task ->
          (match Scheduler.Task.k task () with
          | () -> ()
          | exception e ->
              Printf.eprintf "Exception not handled: %s\n%s%!"
                (Printexc.to_string e)
                (Printexc.get_backtrace ());
              w.last_exc <- Some (Printexc.to_string e)
          | effect Get_worker_id, k -> Effect.Deep.continue k w.id
          | effect Yield, k ->
              Scheduler.enqueue_runnable ~worker_id:w.id (upd_k task k ())
                scheduler
          | effect Sleep until, k ->
              Io_waiter.enqueue_sleeping (upd_k task k ()) until io_waiter
          | effect Wait_write fd, k ->
              Io_waiter.enqueue_writing (upd_k task k ()) fd io_waiter
          | effect Wait_read fd, k ->
              Io_waiter.enqueue_reading (upd_k task k ()) fd io_waiter
          | effect Send_to_channel (item, ch), k ->
              Channel.Select.(f None [ Send (ch, item, Fun.id) ])
                k task scheduler
          | effect Recv_from_channel ch, k ->
              Channel.Select.(f None [ Recv (ch, Fun.id) ]) k task scheduler
          | effect Select (default, specs), k ->
              Channel.Select.f default specs k task scheduler
          | effect Close_channel ch, k -> Channel.close k scheduler ch);
          loop ()
    in
    Util.expect_no_exn loop

  let make ~id = { id; dom = None; last_exc = None }

  let spawn scheduler io_waiter w =
    match w.dom with
    | Some _ -> failwith "worker is already started"
    | None -> w.dom <- Some (Domain.spawn (main scheduler io_waiter w))

  let join w =
    (match w.dom with
    | None -> failwith "worker is not started"
    | Some dom -> Domain.join dom);
    w.last_exc |> Option.iter failwith
end

type t = { scheduler : Scheduler.t; workers : Worker.t list }

let make ?(num_workers = Domain.recommended_domain_count ()) () =
  let scheduler = Scheduler.make ~num_workers in
  let workers = List.init num_workers (fun id -> Worker.make ~id) in
  { scheduler; workers }

let enqueue_task ?worker_id k r =
  Scheduler.enqueue_runnable ?worker_id (Scheduler.Task.make ~k) r.scheduler;
  ()

let run r f =
  let io_waiter = Io_waiter.start r.scheduler in
  r.workers |> List.iter (Worker.spawn r.scheduler io_waiter);
  let mutex = Mutex.create () in
  let cond = Condition.create () in
  let finished = ref false in
  let final_result = ref None in
  r
  |> enqueue_task (fun () ->
         Fun.protect ~finally:(fun () ->
             Mutex.lock mutex;
             finished := true;
             Condition.signal cond;
             Mutex.unlock mutex)
         @@ fun () ->
         let result = f () in
         Mutex.lock mutex;
         final_result := Some result;
         Mutex.unlock mutex);
  Mutex.lock mutex;
  while not !finished do
    Condition.wait cond mutex
  done;

  (* Terminate workers *)
  Scheduler.terminate r.scheduler;
  Io_waiter.terminate io_waiter;
  r.workers |> List.iter Worker.join;

  Option.get !final_result

module Chan = struct
  include Channel
  module Select = Channel.Select

  let make size =
    if size < 0 then invalid_arg "Chan.make: size should be >=0"
    else Channel.make size

  let send v ch = Effect.perform (Send_to_channel (v, ch))
  let recv ch = Effect.perform (Recv_from_channel ch)
  let select ?default specs = Effect.perform (Select (default, specs))
  let close ch = Effect.perform (Close_channel ch)
end

let global_instance = ref None

let initialize f =
  global_instance := Some (make ());
  run (Option.get !global_instance) f

let go f =
  enqueue_task
    ~worker_id:(Effect.perform Get_worker_id)
    f
    (Option.get !global_instance)

let yield () = Effect.perform Yield
let sleep_s dur = Effect.perform (Sleep (Unix.gettimeofday () +. dur))
let wait_write fd = Effect.perform (Wait_write fd)
let wait_read fd = Effect.perform (Wait_read fd)

let socket ?cloexec socket_domain socket_type protocol_type =
  let fd = Unix.socket ?cloexec socket_domain socket_type protocol_type in
  try
    Unix.set_nonblock fd;
    fd
  with Unix.Unix_error _ as e ->
    (try Unix.close fd with Unix.Unix_error _ -> ());
    raise e

let connect fd sockaddr =
  try Unix.connect fd sockaddr
  with Unix.(Unix_error (EINPROGRESS, _, _)) -> (
    wait_write fd;
    match Unix.getsockopt_error fd with
    | None -> ()
    | Some error -> raise (Unix.Unix_error (error, "connect", "")))

let rec accept ?cloexec fd =
  match Unix.accept ?cloexec fd with
  | (fd, _) as ret ->
      Unix.set_nonblock fd;
      ret
  | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
      wait_read fd;
      accept ?cloexec fd

let rec write fd buf offset length =
  try Unix.write fd buf offset length
  with Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
    wait_write fd;
    write fd buf offset length

let rec read fd buf offset length =
  try Unix.read fd buf offset length
  with Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
    wait_read fd;
    read fd buf offset length
