open Entity
open Helper

let parse_req req = req |> Yume.Server.query "acct" |> parse_webfinger_address

let get _ req =
  let username, domain = parse_req req in
  let a =
    try Db.e (Model.Account.get_one ~domain ~username)
    with Sqlx.Error.NoRowFound -> Yume.Server.raise_error_response `Not_found
  in
  make_account_from_model a |> yojson_of_account |> respond_yojson
