type user = {
  id : int;
  email : string;
  created_at : Ptime.t;
  updated_at : Ptime.t;
}

val initialize : unit -> unit
val migrate : unit -> unit Lwt.t
val rollback : unit -> unit Lwt.t
val get_user : username:string -> user Lwt.t
