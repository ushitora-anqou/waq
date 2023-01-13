open Util
module Pg = Postgresql

exception Error of string

type connection = { c : Pg.connection } [@@deriving make]

let failwithf f = Printf.ksprintf (fun s -> raise @@ Error s) f

let rec finish_conn socket_fd connect_poll = function
  | Pg.Polling_failed ->
      Log.debug (fun m -> m "Polling failed");
      Lwt.return_unit
  | Polling_ok ->
      Log.debug (fun m -> m "Polling ok");
      Lwt.return_unit
  | Polling_reading ->
      Log.debug (fun m -> m "Polling reading");
      ignore_lwt @@ Lwt.choose [ Lwt_unix.wait_read socket_fd ];%lwt
      finish_conn socket_fd connect_poll (connect_poll ())
  | Polling_writing ->
      Log.debug (fun m -> m "Polling writing");
      ignore_lwt @@ Lwt.choose [ Lwt_unix.wait_write socket_fd ];%lwt
      finish_conn socket_fd connect_poll (connect_poll ())

let wait_for_result (c : Pg.connection) =
  c#consume_input;
  while%lwt c#is_busy do
    let socket_fd = c#socket |> Obj.magic |> Lwt_unix.of_unix_file_descr in
    ignore_lwt @@ Lwt.choose [ Lwt_unix.wait_read socket_fd ];%lwt
    Lwt.return c#consume_input
  done

let fetch_result (c : Pg.connection) =
  wait_for_result c;%lwt
  Lwt.return c#get_result

let fetch_single_result (c : Pg.connection) =
  match%lwt fetch_result c with
  | None -> assert false
  | Some r ->
      let%lwt r' = fetch_result c in
      assert (r' = None);
      Lwt.return r

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
  | exception Pg.Error err ->
      failwithf "Pg connection failed (1): %s" (Pg.string_of_error err)
  | c ->
      finish_conn
        (c#socket |> Obj.magic |> Lwt_unix.of_unix_file_descr)
        (fun () -> c#connect_poll)
        Polling_writing;%lwt
      if c#status = Bad then
        failwithf "Pg connection failed (2): %s" c#error_message;
      assert (c#status = Ok);
      c#set_nonblocking true;
      Lwt.return @@ make_connection ~c

let execute (c : connection) (sql : string) : unit Lwt.t =
  c.c#send_query sql;
  let%lwt r = fetch_single_result c.c in
  match r#status with
  | Command_ok -> Lwt.return_unit
  | _ ->
      failwithf "Pg execution failed (%s): %s"
        (Pg.Error_code.to_string r#error_code)
        r#error
