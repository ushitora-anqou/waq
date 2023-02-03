open Util
module Pg = Postgresql

exception Error of string
exception NoRowFound

let failwithf f = Printf.ksprintf (fun s -> raise @@ Error s) f

module Value = struct
  type t =
    [ `Null
    | `String of string
    | `Int of int
    | `Float of float
    | `Timestamp of Ptime.t ]
  [@@deriving show]

  type null_t = [ t | `NullString of string option ]

  let normalize : null_t -> t = function
    | `NullString None -> `Null
    | `NullString (Some s) -> `String s
    | #t as v -> v

  let to_string = show

  let expect_int : t -> int = function
    | `Int i -> i
    | v -> failwithf "Expect int, got: %s" (to_string v)

  let expect_int_opt : t -> int option = function
    | `Null -> None
    | `Int i -> Some i
    | v -> failwithf "Expect int or null, got: %s" (to_string v)

  let expect_string : t -> string = function
    | `String s -> s
    | v -> failwithf "Expect string, got: %s" (to_string v)

  let expect_string_opt : t -> string option = function
    | `Null -> None
    | `String s -> Some s
    | v -> failwithf "Expect string or null, got: %s" (to_string v)

  let expect_timestamp : t -> Ptime.t = function
    | `Timestamp t -> t
    | v -> failwithf "Expect timestamp, got: %s" (to_string v)

  let expect_timestamp_opt : t -> Ptime.t option = function
    | `Null -> None
    | `Timestamp t -> Some t
    | v -> failwithf "Expect timestamp or null, got: %s" (to_string v)
end

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
end

module Make (D : Driver) = struct
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
    match res with
    | [ row ] -> Lwt.return row
    | [] -> raise NoRowFound
    | _ -> failwithf "Expected only one row but got many"

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

  let named_query ?(p = []) (c : connection) (sql : string) : query_result Lwt.t
      =
    let p, sql = resolve_named_sql p sql in
    query ~p c sql

  let named_query_row ?(p = []) (c : connection) (sql : string) :
      single_query_result Lwt.t =
    let p, sql = resolve_named_sql p sql in
    query_row ~p c sql
end

module PgDriver = struct
  type connection = { c : Pg.connection } [@@deriving make]

  type statement = {
    name : string;
    params : Pg.ftype list;
    fields : (string * Pg.ftype) list;
  }
  [@@deriving make]

  let ftype_mapper (ty : Pg.ftype) f =
    match ty with
    | TEXT | VARCHAR -> f `String
    | INT2 | INT4 | INT8 -> f `Int
    | FLOAT4 | FLOAT8 -> f `Float
    | TIMESTAMP | TIMESTAMPTZ -> f `Timestamp
    | _ ->
        failwithf "Not implemented ftype in ftype_mapper: %s"
          (Pg.string_of_ftype ty)

  let value_to_string_for_pg ~(ty : Pg.ftype) (v : value) : string =
    ftype_mapper ty @@ fun ty' ->
    match (ty', v) with
    | _, `Null -> Pg.null
    | `String, `String s -> s
    | `Int, `Int i -> string_of_int i
    | `Float, `Float f -> string_of_float f
    | `Timestamp, `Timestamp t -> Ptime.to_rfc3339 ~frac_s:6 t
    | _ ->
        failwithf "Invalid pair of type and value: \"%s\" and \"%s\""
          (Pg.string_of_ftype ty) (Value.to_string v)

  let value_of_string_for_pg ~(ty : Pg.ftype) (s : string option) : value =
    let aux s = function
      | `String -> `String s
      | `Int -> `Int (int_of_string s)
      | `Float -> `Float (float_of_string s)
      | `Timestamp -> (
          (* e.g., 2023-01-13 14:02:17.4242
                   012345678901234567890123 *)
          match Ptime.of_rfc3339 (s ^ "Z") with
          | Ok (t, _, _) -> `Timestamp t
          | Error _ -> failwithf "Invalid format of timestamp: %s" s)
      | _ -> failwithf "Not implemented pg datatype: %s" (Pg.string_of_ftype ty)
    in
    s |> Option.fold ~none:`Null ~some:(fun s -> ftype_mapper ty (aux s))

  let raise_error msg = function
    | `PgError (e : Pg.error) ->
        failwithf "Pg %s failed: %s" msg (Pg.string_of_error e)
    | `Result (r : Pg.result) ->
        failwithf "Pg %s failed (%s): %s" msg
          (Pg.Error_code.to_string r#error_code)
          r#error

  let rec finish_conn socket_fd connect_poll = function
    | Pg.Polling_failed ->
        (*Logq.debug (fun m -> m "Polling failed");*)
        Lwt.return_unit
    | Polling_ok ->
        (*Logq.debug (fun m -> m "Polling ok");*)
        Lwt.return_unit
    | Polling_reading ->
        (*Logq.debug (fun m -> m "Polling reading");*)
        ignore_lwt @@ Lwt.choose [ Lwt_unix.wait_read socket_fd ];%lwt
        finish_conn socket_fd connect_poll (connect_poll ())
    | Polling_writing ->
        (*Logq.debug (fun m -> m "Polling writing");*)
        ignore_lwt @@ Lwt.choose [ Lwt_unix.wait_write socket_fd ];%lwt
        finish_conn socket_fd connect_poll (connect_poll ())

  let wait_for_result (c : Pg.connection) =
    c#consume_input;
    while%lwt c#is_busy do
      let socket_fd = c#socket |> Obj.magic |> Lwt_unix.of_unix_file_descr in
      ignore_lwt @@ Lwt.choose [ Lwt_unix.wait_read socket_fd ];%lwt
      Lwt.return c#consume_input
    done

  let fetch_result (c : connection) =
    try%lwt
      wait_for_result c.c;%lwt
      Lwt.return c.c#get_result
    with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e

  let fetch_single_result (c : connection) =
    match%lwt fetch_result c with
    | None -> assert false
    | Some r ->
        let%lwt r' = fetch_result c in
        assert (r' = None);
        Lwt.return r

  let send_query (c : connection) (sql : string) : unit =
    try c.c#send_query sql
    with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e

  let send_prepare =
    let index = ref 0 in
    fun (c : connection) (sql : string) : string ->
      let name = "prepared_statement_" ^ string_of_int !index in
      index := !index + 1;
      try
        c.c#send_prepare name sql;
        name
      with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e

  let send_query_prepared (c : connection) (stmt : statement)
      (params : value list) =
    let params =
      List.combine stmt.params params
      |> List.map (fun (ty, v) -> value_to_string_for_pg ~ty v)
      |> Array.of_list
    in
    try c.c#send_query_prepared ~params stmt.name
    with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e

  let send_describe_prepared (c : connection) (name : string) =
    try c.c#send_describe_prepared name
    with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e

  let expect_command_ok (name : string) (r : Pg.result Lwt.t) : Pg.result Lwt.t
      =
    let%lwt r = r in
    match r#status with
    | Command_ok -> Lwt.return r
    | _ -> raise_error name @@ `Result r

  let execute_direct (c : connection) (sql : string) : unit Lwt.t =
    send_query c sql;
    ignore_lwt @@ expect_command_ok __FUNCTION__ @@ fetch_single_result c

  let prepare (c : connection) (sql : string) : statement Lwt.t =
    (* Prepare *)
    let name = send_prepare c sql in
    ignore_lwt @@ expect_command_ok __FUNCTION__ @@ fetch_single_result c;%lwt

    (* Describe *)
    send_describe_prepared c name;
    let%lwt desc = expect_command_ok __FUNCTION__ @@ fetch_single_result c in
    let params = desc#nparams |> iota |> List.map desc#paramtype in
    let fields =
      desc#nfields |> iota |> List.map (fun i -> (desc#fname i, desc#ftype i))
    in

    let stmt = make_statement ~name ~params ~fields () in
    Lwt.return stmt

  let execute_stmt (c : connection) (stmt : statement) (params : value list) :
      unit Lwt.t =
    send_query_prepared c stmt params;
    ignore_lwt @@ expect_command_ok __FUNCTION__ @@ fetch_single_result c

  let query_stmt (c : connection) (stmt : statement) (params : value list) :
      query_result Lwt.t =
    send_query_prepared c stmt params;
    let%lwt r = fetch_single_result c in
    if r#status <> Tuples_ok then raise_error __FUNCTION__ @@ `Result r;
    assert (List.length stmt.fields = r#nfields);

    r#ntuples |> iota
    |> List.map (fun row ->
           stmt.fields
           |> List.mapi (fun col (name, ty) ->
                  let s =
                    if r#getisnull row col then None
                    else Some (r#getvalue row col)
                  in
                  (name, value_of_string_for_pg ~ty s)))
    |> Lwt.return

  let connect (uri : string) =
    let u = Uri.of_string uri in
    let host = Uri.host u |> Option.value ~default:"127.0.0.1" in
    let port = Uri.port u |> Option.value ~default:5432 |> string_of_int in
    let user = Uri.user u |> Option.value ~default:(Unix.getlogin ()) in
    let password = Uri.password u |> Option.value ~default:"" in
    let dbname =
      let s = Uri.path u in
      String.sub s 1 (String.length s - 1)
    in
    match
      new Pg.connection ~host ~port ~dbname ~user ~password ~startonly:true ()
    with
    | exception Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e
    | c ->
        finish_conn
          (c#socket |> Obj.magic |> Lwt_unix.of_unix_file_descr)
          (fun () -> c#connect_poll)
          Polling_writing;%lwt
        if c#status = Bad then
          failwithf "Pg connection failed (2): %s" c#error_message;
        assert (c#status = Ok);
        c#set_nonblocking true;
        let c = make_connection ~c in
        execute_direct c "SET TimeZone TO 'UTC'";%lwt
        Lwt.return c

  let disconnect (c : connection) : unit Lwt.t =
    (try c.c#finish with Pg.Error e -> raise_error __FUNCTION__ @@ `PgError e);
    Lwt.return_unit

  let validate (c : connection) : bool Lwt.t =
    c.c#consume_input;
    match c.c#status with
    | (exception Pg.Error _) | Pg.Ok -> Lwt.return_false
    | _ -> Lwt.return_true

  let check (c : connection) f =
    f
    @@
    match c.c#status with
    | exception Pg.Error _ -> false
    | Pg.Ok -> true
    | _ -> false
end

include Make (PgDriver)
