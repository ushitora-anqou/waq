module type S = sig
  module Task : sig
    type t

    val make : k:(unit -> unit) -> t
    val k : t -> unit -> unit
    val with_k : (unit -> unit) -> t -> t
  end

  type t

  val make : num_workers:int -> t
  val enqueue_runnable : ?worker_id:int -> Task.t -> t -> unit
  val dequeue_runnable : worker_id:int -> t -> [ `Ok of Task.t | `Terminated ]
  val terminate : t -> unit
end
