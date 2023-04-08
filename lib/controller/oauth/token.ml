open Util
open Helper

type res = {
  access_token : string;
  token_type : string;
  scope : string;
  created_at : int;
}
[@@deriving make, yojson]

let post req =
  let%lwt grant_type = req |> Httpq.Server.query "grant_type" in
  let%lwt code = req |> Httpq.Server.query_opt "code" in
  let%lwt client_id = req |> Httpq.Server.query "client_id" in
  let%lwt client_secret = req |> Httpq.Server.query "client_secret" in
  let%lwt redirect_uri = req |> Httpq.Server.query "redirect_uri" in
  let%lwt scope = req |> Httpq.Server.query ~default:"read" "scope" in

  let%lwt token =
    match (grant_type, code) with
    | "authorization_code", Some code ->
        let%lwt grant = Oauth_helper.authenticate_access_grant code in
        (* FIXME: Check if scope is correct *)
        let%lwt app =
          Db.(
            e OAuthApplication.(get_one ~id:(Option.get grant#application_id)))
        in
        if app#redirect_uri <> redirect_uri then
          Httpq.Server.raise_error_response `Bad_request;
        if app#uid <> client_id || app#secret <> client_secret then
          Httpq.Server.raise_error_response `Unauthorized;
        if
          let open Ptime in
          grant#expires_in
          < (diff (now ()) grant#created_at |> Span.to_int_s |> Option.get)
        then Httpq.Server.raise_error_response `Bad_request;

        Oauth_helper.generate_access_token ~scopes:scope
          ~resource_owner_id:(Option.get grant#resource_owner_id)
          ~app ()
    | "client_credentials", None ->
        let%lwt app = Db.(e OAuthApplication.(get_one ~uid:client_id)) in
        if app#secret <> client_secret then raise_error_response `Unauthorized;
        Oauth_helper.generate_access_token ~scopes:scope ~app ()
    | _ -> raise_error_response `Bad_request
  in

  make_res ~access_token:token#token ~token_type:"Bearer" ~scope
    ~created_at:
      (token#created_at |> Ptime.to_span |> Ptime.Span.to_int_s |> Option.get)
  |> yojson_of_res |> Yojson.Safe.to_string
  |> Httpq.Server.respond ~headers:[ Helper.content_type_app_json ]
