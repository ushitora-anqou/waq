open Activity
open Lwt.Infix

let get req =
  let _username = req |> Httpq.Server.param ":name" in
  let status_id = req |> Httpq.Server.param ":id" |> int_of_string in
  match%lwt Db.Status.get_one ~id:status_id () with
  | exception Sql.NoRowFound -> Httpq.Server.raise_error_response `Not_found
  | s ->
      note_of_status s >|= of_note >|= to_yojson >|= Yojson.Safe.to_string
      >>= Httpq.Server.respond
