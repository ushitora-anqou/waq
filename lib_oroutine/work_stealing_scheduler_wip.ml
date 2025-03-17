module Task = struct
  type t = { id : int; k : unit -> unit }

  let make ~k = { id = Util.genid (); k }
  let id t = t.id
  let k t = t.k
  let with_k k t = { t with k }
end

module Global_run_queue = struct
  type t = { mutex : Mutex.t; condition : Condition.t; queue : Task.t Queue.t }

  let make () =
    {
      mutex = Mutex.create ();
      condition = Condition.create ();
      queue = Queue.create ();
    }

  let enqueue_one task q =
    Mutex.lock q.mutex;
    Queue.add task q.queue;
    Condition.signal q.condition;
    Mutex.unlock q.mutex;
    ()

  let transfer ~to_ q =
    Mutex.lock q.mutex;
    while Queue.is_empty q.queue do
      Condition.wait q.condition q.mutex
    done;
    Queue.transfer q.queue to_;
    Mutex.unlock q.mutex;
    ()
end

module Local_run_queue = struct
  type t = { mutex : Mutex.t; queue : Task.t Queue.t }

  let make () = { mutex = Mutex.create (); queue = Queue.create () }

  let enqueue_one task q =
    Mutex.lock q.mutex;
    Queue.add task q.queue;
    Mutex.unlock q.mutex;
    ()

  let enqueue_many tasks q =
    Mutex.lock q.mutex;
    Queue.add_seq q.queue tasks;
    Mutex.unlock q.mutex;
    ()

  let dequeue_one q =
    Mutex.lock q.mutex;
    let v = Queue.take_opt q.queue in
    Mutex.unlock q.mutex;
    v

  let steal q =
    Mutex.lock q.mutex;
    let stealed = ref [] in
    for _ = 0 to (Queue.length q.queue / 2) - 1 do
      stealed := Queue.take q.queue :: !stealed
    done;
    let v = List.rev !stealed in
    Mutex.unlock q.mutex;
    v
end

type t = {
  global_run_queue : Global_run_queue.t;
  local_run_queues : Local_run_queue.t array;
}

let make ~num_workers =
  {
    global_run_queue = Global_run_queue.make ();
    local_run_queues = Array.init num_workers (fun _ -> Local_run_queue.make ());
  }

let enqueue_runnable ?worker_id task s =
  match worker_id with
  | None -> Global_run_queue.enqueue_one task s.global_run_queue
  | Some worker_id ->
      Local_run_queue.enqueue_one task s.local_run_queues.(worker_id)

(* Knuth shuffle *)
let shuffle a =
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i + 1) in
    let t = a.(j) in
    a.(j) <- a.(i);
    a.(i) <- t
  done

let dequeue_runnable ~worker_id s =
  (* try to take a task from the local run queue *)
  match Local_run_queue.dequeue_one s.local_run_queues.(worker_id) with
  | Some v -> v
  | None -> (
      (* try to steal tasks from other local run queues *)
      let worker_ids = Array.init (Array.length s.local_run_queues) Fun.id in
      shuffle worker_ids;
      let rec try_steal i =
        if i = Array.length worker_ids then None
        else if worker_ids.(i) = worker_id then try_steal (i + 1)
        else
          match Local_run_queue.steal s.local_run_queues.(worker_ids.(i)) with
          | [] -> try_steal (i + 1)
          | [ task ] -> Some task
          | task :: stealed ->
              Local_run_queue.enqueue_many (List.to_seq stealed)
                s.local_run_queues.(worker_id);
              Some task
      in
      match try_steal 0 with
      | Some v -> v
      | None ->
          (* get tasks from the global run queue *)
          let q = Queue.create () in
          Global_run_queue.transfer ~to_:q s.global_run_queue;
          let v = Queue.take q in
          Local_run_queue.enqueue_many (Queue.to_seq q)
            s.local_run_queues.(worker_id);
          v)
