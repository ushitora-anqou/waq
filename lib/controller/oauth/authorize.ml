let post req =
  let response_type = req |> Httpq.Server.query "response_type" in
  let client_id = req |> Httpq.Server.query "client_id" in
  let redirect_uri = req |> Httpq.Server.query "redirect_uri" in
  let scope = req |> Httpq.Server.query ~default:"read" "scope" in
  let username = req |> Httpq.Server.query "username" in

  if response_type <> "code" then Httpq.Server.raise_error_response `Bad_request;
  let%lwt app = Oauth_helper.authenticate_application client_id in
  (* FIXME: Check if scope is correct *)
  if redirect_uri <> app.redirect_uri then
    Httpq.Server.raise_error_response `Bad_request;

  let%lwt resource_owner_id =
    match%lwt
      Db.(Account.get_one ~domain:None ~username () |> maybe_no_row)
    with
    | None -> Httpq.Server.raise_error_response `Bad_request
    | Some a ->
        let%lwt u = Db.User.get_one ~account_id:a.id () in
        Lwt.return u.id
  in

  let%lwt grant =
    Oauth_helper.generate_access_grant ~expires_in:600 ~redirect_uri
      ~scopes:scope ~app ~resource_owner_id
  in

  if grant.redirect_uri = "urn:ietf:wg:oauth:2.0:oob" then
    Httpq.Server.respond grant.token
  else
    let u = Uri.of_string grant.redirect_uri in
    let u = Uri.add_query_param u ("code", [ grant.token ]) in
    Httpq.Server.respond ~status:`Found
      ~headers:[ (`Location, Uri.to_string u) ]
      ""

let get req =
  let response_type = req |> Httpq.Server.query "response_type" in
  let client_id = req |> Httpq.Server.query "client_id" in
  let redirect_uri = req |> Httpq.Server.query "redirect_uri" in
  let scope = req |> Httpq.Server.query ~default:"read" "scope" in

  Httpq.Server.respond ~status:`OK
    (String.trim
    @@ Jingoo.Jg_template.from_string
         ~models:
           [
             ("response_type", Tstr response_type);
             ("client_id", Tstr client_id);
             ("redirect_uri", Tstr redirect_uri);
             ("scope", Tstr scope);
           ]
         {|
<html>
<body>
<form action="/oauth/authorize" method="post">
  <input type="hidden" name="response_type" value="{{ response_type }}">
  <input type="hidden" name="client_id" value="{{ client_id }}">
  <input type="hidden" name="redirect_uri" value="{{ redirect_uri }}">
  <input type="hidden" name="scope" value="{{ scope }}">
  <div>
    <label for="username">Username: </label>
    <input type="text" name="username" id="username" required>
  </div>
  <div>
    <label for="password">Password: </label>
    <input type="password" name="password" id="password" required>
  </div>
  <div>
    <input type="submit" value="Submit">
  </div>
</form>
</body>
</html>|}
    )
