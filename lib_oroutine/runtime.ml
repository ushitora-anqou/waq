module type S = sig
  val initialize : (unit -> 'a) -> 'a
  val go : (unit -> unit) -> unit
  val yield : unit -> unit
  val sleep_s : float -> unit
  val wait_write : Unix.file_descr -> unit
  val wait_read : Unix.file_descr -> unit

  val socket :
    ?cloexec:bool ->
    Unix.socket_domain ->
    Unix.socket_type ->
    int ->
    Unix.file_descr

  val accept :
    ?cloexec:bool -> Unix.file_descr -> Unix.file_descr * Unix.sockaddr

  val connect : Unix.file_descr -> Unix.sockaddr -> unit
  val write : Unix.file_descr -> bytes -> int -> int -> int
  val read : Unix.file_descr -> bytes -> int -> int -> int

  module Chan : sig
    type 'a t
    type recv_error = [ `Closed ]
    type 'a recv_result = ('a, recv_error) result

    module Select : sig
      type ('a, 'b) spec =
        | Send of ('a t * 'a * (unit -> 'b))
        | Recv of ('a t * ('a recv_result -> 'b))

      type ('ty, 'v, 'b) t =
        | [] : ('v, 'v, 'b) t
        | ( :: ) :
            ('a, 'b) spec * ('ty, 'v, 'b) t
            -> (('a, 'b) spec -> 'ty, 'v, 'b) t
    end

    val make : int -> 'a t
    val close : 'a t -> unit
    val send : 'a -> 'a t -> unit
    val recv : 'a t -> 'a recv_result
    val select : ?default:(unit -> 'b) -> ('ty, 'v, 'b) Select.t -> 'b
  end
end

module Make (Scheduler : Scheduler.S) : S = struct
  module Channel = struct
    type 'a t = {
      mutex : Mutex.t;
      mutable closed : bool;
      queued_items : 'a Queue.t;
      pending_recv :
        (bool Atomic.t (* canceled? *) * ('a -> Scheduler.Task.t)) Queue.t;
      pending_send :
        (bool Atomic.t (* canceled? *) * 'a * Scheduler.Task.t) Queue.t;
      bound_size : int;
    }

    type recv_error = [ `Closed ]
    type 'a recv_result = ('a, recv_error) result

    let make size =
      assert (size > 0);
      {
        mutex = Mutex.create ();
        closed = false;
        queued_items = Queue.create ();
        pending_recv = Queue.create ();
        pending_send = Queue.create ();
        bound_size = size;
      }

    let close ch =
      Mutex.lock ch.mutex;
      ch.closed <- true;
      Mutex.unlock ch.mutex

    let check_invariant ch =
      if Queue.length ch.queued_items = 0 then
        assert (Queue.length ch.pending_send = 0)
      else assert (Queue.length ch.pending_recv = 0)

    module Select = struct
      type ('a, 'b) spec =
        | Send of ('a t * 'a * (unit -> 'b))
        | Recv of ('a t * ('a recv_result -> 'b))

      type ('ty, 'v, 'b) t =
        | [] : ('v, 'v, 'b) t
        | ( :: ) :
            ('a, 'b) spec * ('ty, 'v, 'b) t
            -> (('a, 'b) spec -> 'ty, 'v, 'b) t

      let select_send_spec k task scheduler canceled ch item handler =
        Mutex.lock ch.mutex;
        if ch.closed then (
          Mutex.unlock ch.mutex;
          failwith "Channel.Select.select_send_spec: closed channel")
        else (
          check_invariant ch;
          if Queue.length ch.queued_items >= ch.bound_size then (
            (* If the queue is full, add k and handler to ch.pending_send
             to block their process. Then, continue to iterate other specs. *)
            let task =
              task
              |> Scheduler.Task.with_k (fun () ->
                     Effect.Deep.continue k (handler ()))
            in
            ch.pending_send |> Queue.add (canceled, item, task);
            Mutex.unlock ch.mutex;
            `Loop)
          else
            (* If the queue has some space, accept the item. If ch.pending_recv
             is not empty, hand over the item directly to the waiter.
             After that, continue k. Note that
             in this branch we don't have to continue to iterate other specs
             because it's obvious that other specs won't be selected anymore. *)
            match Atomic.compare_and_set canceled false true with
            | false ->
                Mutex.unlock ch.mutex;
                `Finish
            | true ->
                let rec resolve_pending_recv () =
                  match Queue.take_opt ch.pending_recv with
                  | None -> Queue.add item ch.queued_items
                  | Some (canceled, gen_task) -> (
                      match Atomic.compare_and_set canceled false true with
                      | false -> resolve_pending_recv ()
                      | true ->
                          scheduler
                          |> Scheduler.enqueue_runnable (gen_task item))
                in
                resolve_pending_recv ();
                Mutex.unlock ch.mutex;
                `Continue (handler ()))

      let select_recv_spec k task scheduler canceled ch handler =
        Mutex.lock ch.mutex;
        if ch.closed then (
          Mutex.unlock ch.mutex;
          `Continue (handler (Error `Closed)))
        else (
          check_invariant ch;
          if Queue.length ch.queued_items = 0 then (
            let task item =
              (* Note that ch.mutex isn't necessarily locked in this function. *)
              task
              |> Scheduler.Task.with_k (fun () ->
                     Effect.Deep.continue k (handler (Ok item)))
            in
            ch.pending_recv |> Queue.add (canceled, task);
            Mutex.unlock ch.mutex;
            `Loop)
          else
            match Atomic.compare_and_set canceled false true with
            | false ->
                Mutex.unlock ch.mutex;
                `Finish
            | true ->
                let rec resolve_pending_send () =
                  match Queue.take_opt ch.pending_send with
                  | None -> ()
                  | Some (canceled, item, task) -> (
                      match Atomic.compare_and_set canceled false true with
                      | false -> resolve_pending_send ()
                      | true ->
                          ch.queued_items |> Queue.push item;
                          scheduler |> Scheduler.enqueue_runnable task)
                in
                resolve_pending_send ();
                let item = Queue.pop ch.queued_items in
                Mutex.unlock ch.mutex;
                `Continue (handler (Ok item)))

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
            match
              select_send_spec k task scheduler canceled ch item handler
            with
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
             (fun a (_, t) ->
               Some (match a with None -> t | Some a -> min a t))
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
      if is_ready (Eventfd.fd_read_to_poll state.efd) then
        Eventfd.clear state.efd;

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
    type t = { id : int; mutable dom : unit Domain.t option }

    let upd_k task k a =
      Scheduler.Task.with_k (fun () -> Effect.Deep.continue k a) task

    let main scheduler io_waiter w () =
      let rec loop () =
        match Scheduler.dequeue_runnable ~worker_id:w.id scheduler with
        | `Terminated -> ()
        | `Ok task ->
            (match Scheduler.Task.k task () with
            | () -> ()
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
                Channel.Select.f default specs k task scheduler);
            loop ()
      in
      Util.expect_no_exn loop

    let make ~id = { id; dom = None }

    let spawn scheduler io_waiter w =
      match w.dom with
      | Some _ -> failwith "worker is already started"
      | None -> w.dom <- Some (Domain.spawn (main scheduler io_waiter w))

    let join w =
      match w.dom with
      | None -> failwith "worker is not started"
      | Some dom -> Domain.join dom
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
    let final_result = ref None in
    r
    |> enqueue_task (fun () ->
           let result = f () in
           Mutex.lock mutex;
           final_result := Some result;
           Condition.signal cond;
           Mutex.unlock mutex);
    Mutex.lock mutex;
    while !final_result = None do
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
      if size <= 0 then invalid_arg "Chan.make: size should be >0"
      else Channel.make size

    let send v ch = Effect.perform (Send_to_channel (v, ch))
    let recv ch = Effect.perform (Recv_from_channel ch)
    let select ?default specs = Effect.perform (Select (default, specs))
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
end
