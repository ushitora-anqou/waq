open Util
open Helper

type res = {
  access_token : string;
  token_type : string;
  scope : string;
  created_at : int;
}
[@@deriving make, yojson]

let post _ req =
  let grant_type = req |> Yume.Server.query "grant_type" in
  let code = req |> Yume.Server.query_opt "code" in
  let client_id = req |> Yume.Server.query "client_id" in
  let client_secret = req |> Yume.Server.query "client_secret" in
  let redirect_uri = req |> Yume.Server.query "redirect_uri" in
  let scope = req |> Yume.Server.query ~default:"read" "scope" in

  let token =
    match (grant_type, code) with
    | "authorization_code", Some code ->
        let grant = Oauth_helper.authenticate_access_grant code in
        (* FIXME: Check if scope is correct *)
        let app =
          Db.(
            e OAuthApplication.(get_one ~id:(Option.get grant#application_id)))
        in
        if app#redirect_uri <> redirect_uri then
          Yume.Server.raise_error_response `Bad_request;
        if app#uid <> client_id || app#secret <> client_secret then
          Yume.Server.raise_error_response `Unauthorized;
        if
          let open Ptime in
          grant#expires_in
          < (diff (now ()) grant#created_at |> Span.to_int_s |> Option.get)
        then Yume.Server.raise_error_response `Bad_request;

        Oauth_helper.generate_access_token ~scopes:scope
          ~resource_owner_id:(Option.get grant#resource_owner_id)
          ~app ()
    | "client_credentials", None ->
        let app = Db.(e OAuthApplication.(get_one ~uid:client_id)) in
        if app#secret <> client_secret then raise_error_response `Unauthorized;
        Oauth_helper.generate_access_token ~scopes:scope ~app ()
    | _ -> raise_error_response `Bad_request
  in

  make_res ~access_token:token#token ~token_type:"Bearer" ~scope
    ~created_at:
      (token#created_at |> Ptime.to_span |> Ptime.Span.to_int_s |> Option.get)
  |> yojson_of_res |> Yojson.Safe.to_string
  |> Yume.Server.respond ~headers:[ Helper.content_type_app_json ]
