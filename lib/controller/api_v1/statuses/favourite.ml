open Util
open Helper
open Entity
open Lwt.Infix

let post req =
  let%lwt account_id = authenticate_user req in
  let status_id = req |> Httpq.Server.param ":id" |> int_of_string in

  let%lwt status =
    match%lwt Db.Status.get_one ~id:status_id () with
    | exception Sql.NoRowFound -> Httpq.Server.raise_error_response `Not_found
    | s -> Lwt.return s
  in

  (match%lwt Db.Favourite.get_one ~status_id ~account_id () with
  | _ -> Lwt.return_unit
  | exception Sql.NoRowFound ->
      let now = Ptime.now () in
      Db.Favourite.(
        make ~id:0 ~created_at:now ~updated_at:now ~account_id ~status_id
        |> save_one)
      |> ignore_lwt);%lwt

  make_status_from_model ~self_id:account_id status
  >|= status_to_yojson >>= respond_yojson
