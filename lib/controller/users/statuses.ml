open Activity
open Lwt.Infix
open Helper

let get req =
  let _username = req |> Httpq.Server.param ":name" in
  let status_id =
    req |> Httpq.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  match%lwt Db.e (Model.Status.get_one ~id:status_id) with
  | exception Sqlx.Error.NoRowFound ->
      Httpq.Server.raise_error_response `Not_found
  | s -> note_of_status s >|= of_note >|= to_yojson >>= respond_activity_yojson
