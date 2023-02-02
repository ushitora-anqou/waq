open Activity

let get req =
  let%lwt self_id = Helper.authenticate_user req in
  let%lwt a = Db.Account.get_one ~id:self_id () in
  make_credential_account_from_model a
  |> credential_account_to_yojson |> Yojson.Safe.to_string
  |> Http.Server.respond ~headers:[ Helper.content_type_app_json ]
