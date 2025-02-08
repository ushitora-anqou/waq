class type t = object ('a)
  method query :
    ?p:Value.null_t list -> string -> (string * Value.t) list list Lwt.t

  method query_row :
    ?p:Value.null_t list -> string -> (string * Value.t) list Lwt.t

  method execute : ?p:Value.null_t list -> string -> unit Lwt.t

  method named_query :
    ?p:(string * Value.null_t) list ->
    string ->
    (string * Value.t) list list Lwt.t

  method named_query_row :
    ?p:(string * Value.null_t) list -> string -> (string * Value.t) list Lwt.t

  method named_execute : ?p:(string * Value.null_t) list -> string -> unit Lwt.t
  method enqueue_task_after_commit : ('a -> unit Lwt.t) -> unit Lwt.t
  method transaction : ('a -> unit Lwt.t) -> bool Lwt.t
  (*method enqueued_tasks_after_commit : ('a -> unit Lwt.t) list*)
end
