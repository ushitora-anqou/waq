open Util

exception NoRowFound

let maybe_no_row e =
  match%lwt e with
  | exception NoRowFound -> Lwt.return_none
  | res -> Lwt.return_some res

type value = Value.t
type params = value list
type single_query_result = (string * value) list
type query_result = single_query_result list

module type Driver = sig
  type connection
  type statement

  val validate : connection -> bool Lwt.t
  val check : connection -> (bool -> unit) -> unit
  val connect : string -> connection Lwt.t
  val disconnect : connection -> unit Lwt.t
  val prepare : connection -> string -> statement Lwt.t
  val execute_stmt : connection -> statement -> params -> unit Lwt.t
  val query_stmt : connection -> statement -> params -> query_result Lwt.t
  val transaction : connection -> (unit -> unit Lwt.t) -> bool Lwt.t
end

module Make (D : Driver) = struct
  module Internal = struct
    type connection = D.connection
    type connection_pool = connection Lwt_pool.t

    let connect_pool n uri =
      let validate = D.validate in
      let check = D.check in
      let dispose = D.disconnect in
      Lwt_pool.create n ~validate ~check ~dispose (fun () -> D.connect uri)

    let use : connection_pool -> (connection -> 'a Lwt.t) -> 'a Lwt.t =
      Lwt_pool.use

    let log =
      let open String in
      let max_length = 30 in
      let conv = function
        | `Null -> "NULL"
        | `String s ->
            "\""
            ^ ((if length s > max_length then sub s 0 max_length ^ "..." else s)
              |> escaped)
            ^ "\""
        | `Int i -> string_of_int i
        | `Float f -> string_of_float f
        | `Timestamp t -> Ptime.to_rfc3339 t
      in
      fun sql params ->
        Logq.debug (fun m ->
            m "\o033[1;34m%s\o033[0m [%s]"
              (sql |> split_on_char '\n' |> List.map trim
              |> List.filter (( <> ) "")
              |> List.filter (starts_with ~prefix:"--" |.> not)
              |> concat " " |> trim)
              (params |> List.map conv |> concat "; "))

    let execute ?(p = []) (c : connection) (sql : string) : unit Lwt.t =
      let p = p |> List.map Value.normalize in
      log sql p;
      let%lwt stmt = D.prepare c sql in
      D.execute_stmt c stmt p

    let query ?(p = []) c sql =
      let p = p |> List.map Value.normalize in
      log sql p;
      let%lwt stmt = D.prepare c sql in
      D.query_stmt c stmt p

    let query_row ?(p = []) (c : connection) (sql : string) :
        single_query_result Lwt.t =
      let%lwt res = query c sql ~p in
      match res with row :: _ -> Lwt.return row | [] -> raise NoRowFound

    let resolve_named_sql =
      let re = Re.(Pcre.re {|:([_a-zA-Z][_a-zA-Z0-9]*)|} |> compile) in
      fun p sql ->
        let names = ref [] in
        let sql =
          Re.replace ~all:true re
            ~f:(fun g ->
              let name = Re.Group.get g 1 in
              names := name :: !names;
              "$" ^ string_of_int (List.length !names))
            sql
        in
        let p = !names |> List.rev |> List.map (fun k -> List.assoc k p) in
        (p, sql)

    let named_execute ?(p = []) (c : connection) (sql : string) : unit Lwt.t =
      let p, sql = resolve_named_sql p sql in
      execute ~p c sql

    let named_query ?(p = []) (c : connection) (sql : string) :
        query_result Lwt.t =
      let p, sql = resolve_named_sql p sql in
      query ~p c sql

    let named_query_row ?(p = []) (c : connection) (sql : string) :
        single_query_result Lwt.t =
      let p, sql = resolve_named_sql p sql in
      query_row ~p c sql

    let transaction (c : connection) (f : unit -> unit Lwt.t) : bool Lwt.t =
      log "BEGIN TRANSACTION" [];
      D.transaction c f >|= fun r ->
      if r then log "COMMIT TRANSACTION" [] else log "ROLLBACK TRANSACTION" [];
      r
  end

  class connection c =
    object (self : 'a)
      val mutable in_transaction = false
      val mutable enqueued = []

      method query (sql : string) (param : Value.t list)
          : (string * Value.t) list list Lwt.t =
        Internal.query c sql ~p:(param : Value.t list :> Value.null_t list)

      method query_row (sql : string) (param : Value.t list)
          : (string * Value.t) list Lwt.t =
        Internal.query_row c sql ~p:(param : Value.t list :> Value.null_t list)

      method execute (sql : string) (param : Value.t list) : unit Lwt.t =
        Internal.execute c sql ~p:(param : Value.t list :> Value.null_t list)

      method enqueue_task_after_commit (f : 'a -> unit Lwt.t) : unit Lwt.t =
        if in_transaction then Lwt.return (enqueued <- f :: enqueued)
        else f self

      method transaction (f : 'a -> unit Lwt.t) : bool Lwt.t =
        if in_transaction then failwith "Detected nested transaction";
        in_transaction <- true;
        let%lwt res = Internal.transaction c (fun () -> f self) in
        in_transaction <- false;
        if res then
          try%lwt enqueued |> Lwt_list.iter_s (fun f -> f self)
          with _ -> Lwt.return_unit
        else Lwt.return_unit;%lwt
        enqueued <- [];
        Lwt.return res
    end

  let global_pool = ref None

  let initialize url =
    let pool = Internal.connect_pool 10 url in
    global_pool := Some pool

  let e q =
    Internal.use (Option.get !global_pool) @@ fun c -> q (new connection c)

  let transaction f = e (fun c -> c#transaction f)
end
