val initialize : unit -> unit
val migrate : unit -> (unit, string) result Lwt.t
val rollback : unit -> (unit, string) result Lwt.t
