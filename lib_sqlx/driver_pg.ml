open Util
module Pg = Postgresql

type value = Value.t
type query_result = Engine.query_result
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
  | BOOL -> f `Bool
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
  | `Bool, `Bool b -> string_of_bool b
  | _ ->
      failwithf "Invalid pair of type and value: \"%s\" and \"%s\""
        (Pg.string_of_ftype ty) (Value.show v)

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
    | `Bool -> `Bool (if s = "t" then true else false)
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

let expect_command_ok (name : string) (r : Pg.result Lwt.t) : Pg.result Lwt.t =
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
  let user = match Uri.user u with Some x -> x | None -> Unix.getlogin () in
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

let transaction (c : connection) (f : unit -> unit Lwt.t) : bool Lwt.t =
  execute_direct c "BEGIN";%lwt
  try%lwt
    f ();%lwt
    execute_direct c "COMMIT";%lwt
    Lwt.return_true
  with e ->
    Logq.err (fun m ->
        m "Exception raised in transaction: %s\n%s" (Printexc.to_string e)
          (Printexc.get_backtrace ()));
    execute_direct c "ROLLBACK";%lwt
    Lwt.return_false
