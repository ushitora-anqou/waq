open Util

let generate_application ~name ~redirect_uri ~scopes =
  let open Db.OAuthApplication in
  let now = Ptime.now () in
  let uid = Crypto.SecureRandom.unique_token () in
  let secret = Crypto.SecureRandom.unique_token () in
  make ~id:0 ~name ~uid ~secret ~redirect_uri ~scopes ~created_at:now
    ~updated_at:now ()
  |> insert

let generate_access_token ~scopes ~resource_owner_id
    ~(app : Db.OAuthApplication.t) =
  let open Db.OAuthAccessToken in
  let now = Ptime.now () in
  let token = Crypto.SecureRandom.unique_token () in
  make ~id:0 ~token ~created_at:now ~scopes ~application_id:app.id
    ~resource_owner_id ()
  |> insert

let authenticate_access_token token = Db.OAuthAccessToken.get ~by:(`token token)
