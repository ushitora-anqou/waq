open Entity
open Helper
open Lwt.Infix

let parse_req req = req |> Httpq.Server.query "acct" >|= parse_webfinger_address

let get req =
  let%lwt username, domain = parse_req req in
  let%lwt a =
    match%lwt Db.e (Model.Account.get_one ~domain ~username) with
    | a -> Lwt.return a
    | exception Sqlx.Error.NoRowFound ->
        Httpq.Server.raise_error_response `Not_found
  in
  make_account_from_model a >|= yojson_of_account >>= respond_yojson
