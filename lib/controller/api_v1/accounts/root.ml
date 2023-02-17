open Entity
open Helper
open Lwt.Infix

let get req =
  let id = req |> Httpq.Server.param ":id" |> int_of_string in
  match%lwt Db.Account.get_one ~id () with
  | exception Sql.NoRowFound -> Httpq.Server.raise_error_response `Not_found
  | a -> make_account_from_model a >|= yojson_of_account >>= respond_yojson
