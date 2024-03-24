open Activity
open Helper

let get _ req =
  let _username = req |> Yume.Server.param ":name" in
  let status_id =
    req |> Yume.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  match Db.e (Model.Status.get_one ~id:status_id) with
  | exception Sqlx.Error.NoRowFound ->
      Yume.Server.raise_error_response `Not_found
  | s -> note_of_status s |> of_note |> to_yojson |> respond_activity_yojson
