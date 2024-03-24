open Helper

let post _env req =
  let response_type = req |> Yume.Server.query "response_type" in
  let client_id = req |> Yume.Server.query "client_id" in
  let redirect_uri = req |> Yume.Server.query "redirect_uri" in
  let scope = req |> Yume.Server.query ~default:"read" "scope" in
  let state = req |> Yume.Server.query_opt "state" in
  let username = req |> Yume.Server.query "username" in
  let password = req |> Yume.Server.query "password" in

  if response_type <> "code" then raise_error_response `Bad_request;
  let app = Oauth_helper.authenticate_application client_id in
  (* FIXME: Check if scope is correct *)
  if redirect_uri <> app#redirect_uri then raise_error_response `Bad_request;

  let account =
    try Db.(e Account.(get_one ~domain:None ~username))
    with Sqlx.Error.NoRowFound -> raise_error_response `Bad_request
  in
  let user =
    try Db.(e User.(get_one ~account_id:account#id))
    with Sqlx.Error.NoRowFound -> raise_error_response `Bad_request
  in
  if not Bcrypt.(verify password (hash_of_string user#encrypted_password)) then
    raise_error_response `Unauthorized;
  let resource_owner_id = user#id in

  let grant =
    Oauth_helper.generate_access_grant ~expires_in:600 ~redirect_uri
      ~scopes:scope ~app ~resource_owner_id
  in

  if grant#redirect_uri = "urn:ietf:wg:oauth:2.0:oob" then
    Yume.Server.respond grant#token
  else
    let u = Uri.of_string grant#redirect_uri in
    let u = Uri.add_query_param u ("code", [ grant#token ]) in
    let u =
      state
      |> Option.fold ~none:u ~some:(fun s ->
             Uri.add_query_param u ("state", [ s ]))
    in
    Yume.Server.respond ~status:`Found
      ~headers:[ (`Location, Uri.to_string u) ]
      ""

let get _ req =
  let response_type = req |> Yume.Server.query "response_type" in
  let client_id = req |> Yume.Server.query "client_id" in
  let redirect_uri = req |> Yume.Server.query "redirect_uri" in
  let scope = req |> Yume.Server.query ~default:"read" "scope" in
  let state = req |> Yume.Server.query_opt "state" in

  let models =
    let open Jingoo.Jg_types in
    [
      ("response_type", Tstr response_type);
      ("client_id", Tstr client_id);
      ("redirect_uri", Tstr redirect_uri);
      ("scope", Tstr scope);
    ]
  in
  let models =
    state
    |> Option.fold ~none:models ~some:(fun state ->
           ("state", Tstr state) :: models)
  in

  respond_html
    (String.trim
    @@ Jingoo.Jg_template.from_string ~models
         {|
<html>
<body>
<form action="/oauth/authorize" method="post">
  <input type="hidden" name="response_type" value="{{ response_type }}">
  <input type="hidden" name="client_id" value="{{ client_id }}">
  <input type="hidden" name="redirect_uri" value="{{ redirect_uri }}">
  <input type="hidden" name="scope" value="{{ scope }}">
  {% if state %}<input type="hidden" name="state" value="{{ state }}">{% endif %}
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
