let get req =
  let response_type = req |> Httpx.query "response_type" in
  let client_id = req |> Httpx.query "client_id" in
  let redirect_uri = req |> Httpx.query "redirect_uri" in
  let scope = req |> Httpx.query ~default:"read" "scope" in

  if response_type <> "code" then Http.raise_error_response `Bad_request;
  let%lwt app = Oauth.authenticate_application client_id in
  (* FIXME: Check if scope is correct *)
  if redirect_uri <> app.redirect_uri then
    Http.raise_error_response `Bad_request;

  let%lwt grant =
    (* FIXME: Authenticate the user and obtain its correct resource owner id *)
    Oauth.generate_access_grant ~expires_in:600 ~redirect_uri ~scopes:scope ~app
      ~resource_owner_id:1
  in

  if grant.redirect_uri = "urn:ietf:wg:oauth:2.0:oob" then
    Http.respond grant.token
  else
    Http.respond ~status:`Found
      ~headers:[ ("Location", grant.redirect_uri ^ "?code=" ^ grant.token) ]
      ""
