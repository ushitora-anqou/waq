type res = {
  id : string;
  name : string;
  website : string option;
  redirect_uri : string;
  client_id : string;
  client_secret : string;
}
[@@deriving make, yojson]

let post req =
  let client_name = req |> Httpx.query "client_name" in
  let redirect_uris = req |> Httpx.query "redirect_uris" in
  let scopes = req |> Httpx.query ~default:"read" "scopes" in

  let%lwt app =
    Oauth.generate_application ~name:client_name ~redirect_uri:redirect_uris
      ~scopes
  in
  make_res ~id:(string_of_int app.id) ~name:app.name
    ~redirect_uri:app.redirect_uri ~client_id:app.uid ~client_secret:app.secret
    ()
  |> res_to_yojson |> Yojson.Safe.to_string
  |> Httpx.respond ~headers:[ Helper.content_type_app_json ]
