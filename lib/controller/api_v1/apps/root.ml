type res = {
  id : string;
  name : string;
  website : string option;
  redirect_uri : string;
  vapid_key : string;
  client_id : string;
  client_secret : string;
}
[@@deriving make, yojson]

let post _ req =
  let client_name = req |> Yume.Server.query "client_name" in
  let redirect_uris = req |> Yume.Server.query "redirect_uris" in
  let scopes = req |> Yume.Server.query ~default:"read" "scopes" in

  let app =
    Oauth_helper.generate_application ~name:client_name
      ~redirect_uri:redirect_uris ~scopes
  in
  make_res
    ~id:(app#id |> Model.OAuthApplication.ID.to_int |> string_of_int)
    ~name:app#name ~redirect_uri:app#redirect_uri ~client_id:app#uid
    ~client_secret:app#secret
    ~vapid_key:(Config.vapid_public_key ())
    ()
  |> yojson_of_res |> Yojson.Safe.to_string
  |> Yume.Server.respond ~headers:[ Helper.content_type_app_json ]
