module Global_run_queue_scheduler = struct
  module Task = struct
    type t = { id : int; k : unit -> unit }

    let make ~k = { id = Util.genid (); k }
    let id t = t.id
    let k t = t.k
    let with_k k t = { t with k }
  end

  type t = {
    mutex : Mutex.t;
    non_empty : Condition.t;
    runnable : Task.t Queue.t;
    mutable terminate : bool;
  }

  let make ~num_workers:_ =
    {
      mutex = Mutex.create ();
      non_empty = Condition.create ();
      runnable = Queue.create ();
      terminate = false;
    }

  let enqueue_runnable ?worker_id:_ task q =
    Mutex.lock q.mutex;
    Queue.push task q.runnable;
    Condition.signal q.non_empty;
    Mutex.unlock q.mutex;
    ()

  let dequeue_runnable ~worker_id:_ q =
    Mutex.lock q.mutex;
    while Queue.is_empty q.runnable && not q.terminate do
      Condition.wait q.non_empty q.mutex
    done;
    let v = if q.terminate then `Terminated else `Ok (Queue.take q.runnable) in
    Mutex.unlock q.mutex;
    v

  let terminate q =
    Mutex.lock q.mutex;
    q.terminate <- true;
    Condition.broadcast q.non_empty;
    Mutex.unlock q.mutex;
    ()
end

include Global_run_queue_scheduler
