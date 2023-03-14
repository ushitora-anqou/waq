open Entity
open Helper
open Lwt.Infix

let get req =
  let id =
    req |> Httpq.Server.param ":id" |> int_of_string |> Model.Account.ID.of_int
  in
  match%lwt Db.e (Model.Account.get_one ~id) with
  | exception Sqlx.Error.NoRowFound ->
      Httpq.Server.raise_error_response `Not_found
  | a -> make_account_from_model a >|= yojson_of_account >>= respond_yojson
