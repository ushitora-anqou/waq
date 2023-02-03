open Util

type res = {
  access_token : string;
  token_type : string;
  scope : string;
  created_at : int;
}
[@@deriving make, yojson]

let post req =
  let grant_type = req |> Httpq.Server.query "grant_type" in
  let code = req |> Httpq.Server.query "code" in
  let client_id = req |> Httpq.Server.query "client_id" in
  let client_secret = req |> Httpq.Server.query "client_secret" in
  let redirect_uri = req |> Httpq.Server.query "redirect_uri" in
  let scope = req |> Httpq.Server.query ~default:"read" "scope" in

  if grant_type <> "authorization_code" then
    Httpq.Server.raise_error_response `Bad_request;

  let%lwt grant = Oauth_helper.authenticate_access_grant code in
  (* FIXME: Check if scope is correct *)
  let%lwt app =
    Db.OAuthApplication.get_one ~id:(Option.get grant.application_id) ()
  in
  if app.redirect_uri <> redirect_uri then
    Httpq.Server.raise_error_response `Bad_request;
  if app.uid <> client_id || app.secret <> client_secret then
    Httpq.Server.raise_error_response `Unauthorized;
  if
    let open Ptime in
    grant.expires_in
    < (diff (now ()) (Option.get app.created_at) |> Span.to_int_s |> Option.get)
  then Httpq.Server.raise_error_response `Bad_request;

  let%lwt token =
    Oauth_helper.generate_access_token ~scopes:scope
      ~resource_owner_id:(Option.get grant.resource_owner_id)
      ~app
  in

  make_res ~access_token:token.token ~token_type:"Bearer" ~scope
    ~created_at:
      (token.created_at |> Ptime.to_span |> Ptime.Span.to_int_s |> Option.get)
  |> res_to_yojson |> Yojson.Safe.to_string
  |> Httpq.Server.respond ~headers:[ Helper.content_type_app_json ]
