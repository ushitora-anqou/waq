open Util

let generate_application ~name ~redirect_uri ~scopes =
  let open Db.OAuthApplication in
  let now = Ptime.now () in
  let uid = Crypto.SecureRandom.unique_token () in
  let secret = Crypto.SecureRandom.unique_token () in
  make ~id:0 ~name ~uid ~secret ~redirect_uri ~scopes ~created_at:now
    ~updated_at:now ()
  |> save_one

let generate_access_grant ~expires_in ~redirect_uri ~scopes ~resource_owner_id
    ~(app : Db.OAuthApplication.t) =
  let open Db.OAuthAccessGrant in
  let now = Ptime.now () in
  let token = Crypto.SecureRandom.unique_token () in
  make ~id:0 ~token ~expires_in ~redirect_uri ~scopes ~created_at:now
    ~resource_owner_id ~application_id:app.id ()
  |> save_one

let generate_access_token ~scopes ~resource_owner_id
    ~(app : Db.OAuthApplication.t) =
  let open Db.OAuthAccessToken in
  let now = Ptime.now () in
  let token = Crypto.SecureRandom.unique_token () in
  make ~id:0 ~token ~created_at:now ~scopes ~application_id:app.id
    ~resource_owner_id ()
  |> save_one

let authenticate_access_token token = Db.OAuthAccessToken.get_one ~token ()

let authenticate_application uid =
  match%lwt Db.(OAuthApplication.get_one ~uid () |> maybe_no_row) with
  | Some app -> Lwt.return app
  | None -> Http.Server.raise_error_response `Bad_request

let authenticate_access_grant auth_code =
  match%lwt
    Db.(OAuthAccessGrant.get_one ~token:auth_code () |> maybe_no_row)
  with
  | Some grant -> Lwt.return grant
  | None -> Http.Server.raise_error_response `Bad_request
