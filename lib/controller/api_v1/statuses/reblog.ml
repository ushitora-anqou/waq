open Util
open Helper
open Entity
open Lwt.Infix

let post req =
  let%lwt self_id = authenticate_user req in
  let status_id = req |> Httpq.Server.param ":id" |> int_of_string in
  let%lwt status =
    Db.(Status.get_one ~id:status_id () |> maybe_no_row) >|= function
    | Some s -> s
    | None -> Httpq.Server.raise_error_response `Not_found
  in
  let%lwt status =
    match status.reblog_of_id with
    | None -> Lwt.return status
    | Some id -> Db.Status.get_one ~id ()
  in
  let%lwt s =
    match%lwt Db.Status.get_one ~reblog_of_id:(Some status.id) () with
    | s ->
        (* Already reblogged *)
        Lwt.return s
    | exception Sql.NoRowFound ->
        let now = Ptime.now () in
        let%lwt s =
          Db.Status.(
            make ~id:0 ~text:"" ~created_at:now ~updated_at:now
              ~account_id:self_id ~reblog_of_id:status.id ()
            |> save_one_with_uri)
        in
        Service.Distribute.kick s;
        Lwt.return s
  in
  make_status_from_model ~self_id s
  >|= status_to_yojson >>= Helper.respond_yojson
