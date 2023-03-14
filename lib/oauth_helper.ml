open Util

let generate_application ~name ~redirect_uri ~scopes =
  let open Db.OAuthApplication in
  let now = Ptime.now () in
  let uid = Crypto.SecureRandom.unique_token () in
  let secret = Crypto.SecureRandom.unique_token () in
  make ~name ~uid ~secret ~redirect_uri ~scopes ~created_at:now ~updated_at:now
    ()
  |> save_one |> Db.e

let generate_access_grant ~expires_in ~redirect_uri ~scopes ~resource_owner_id
    ~(app : Db.OAuthApplication.t) =
  let open Db.OAuthAccessGrant in
  let now = Ptime.now () in
  let token = Crypto.SecureRandom.unique_token () in
  make ~token ~expires_in ~redirect_uri ~scopes ~created_at:now
    ~resource_owner_id ~application_id:app#id ()
  |> save_one |> Db.e

let generate_access_token ~scopes ~resource_owner_id
    ~(app : Db.OAuthApplication.t) =
  let open Db.OAuthAccessToken in
  let now = Ptime.now () in
  let token = Crypto.SecureRandom.unique_token () in
  make ~token ~created_at:now ~scopes ~application_id:app#id ~resource_owner_id
    ()
  |> save_one |> Db.e

let authenticate_access_token token = Db.(e (OAuthAccessToken.get_one ~token))

let authenticate_application uid =
  match%lwt Db.(e (OAuthApplication.get_one ~uid) |> maybe_no_row) with
  | Some app -> Lwt.return app
  | None -> Httpq.Server.raise_error_response `Bad_request

let authenticate_access_grant auth_code =
  match%lwt
    Db.(e (OAuthAccessGrant.get_one ~token:auth_code) |> maybe_no_row)
  with
  | Some grant -> Lwt.return grant
  | None -> Httpq.Server.raise_error_response `Bad_request
