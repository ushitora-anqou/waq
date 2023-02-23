open Util
open Lwt.Infix

let app_activity_json = "application/activity+json"
let text_html = "text/html"

let content_type_app_xrd_xml =
  (`Content_type, "application/xrd+xml; charset=utf-8")

let content_type_app_jrd_json =
  (`Content_type, "application/jrd+json; charset=utf-8")

let content_type_app_json = (`Content_type, "application/json; charset=utf-8")
let content_type_app_activity_json = (`Content_type, "application/activity+json")

let authenticate_bearer = function
  | Httpq.Server.Request r -> (
      try
        let header = r.headers |> List.assoc `Authorization in
        assert (String.starts_with ~prefix:"Bearer " header);
        let bearer_token = String.sub header 7 (String.length header - 7) in
        Oauth_helper.authenticate_access_token bearer_token
      with _ -> Httpq.Server.raise_error_response `Unauthorized)

let authenticate_user (r : Httpq.Server.request) =
  try
    let%lwt token = authenticate_bearer r in
    token.resource_owner_id |> Option.get |> Lwt.return
  with _ -> Httpq.Server.raise_error_response `Unauthorized

let may_authenticate_user r =
  try%lwt authenticate_user r >|= Option.some with _ -> Lwt.return_none

let int_of_string s =
  match int_of_string_opt s with
  | None -> Httpq.Server.raise_error_response `Bad_request
  | Some i -> i

let bool_of_string s =
  match bool_of_string_opt s with
  | None -> Httpq.Server.raise_error_response `Bad_request
  | Some b -> b

let respond_yojson y =
  Yojson.Safe.to_string y
  |> Httpq.Server.respond ~headers:[ content_type_app_json ]

let respond_activity_yojson y =
  Yojson.Safe.to_string y
  |> Httpq.Server.respond ~headers:[ content_type_app_activity_json ]

let render ~default routes req =
  let accept =
    Httpq.Server.header_opt `Accept req
    |> Option.map (String.split_on_char ',' |.> List.map String.trim)
  in
  let route =
    match accept with
    | None -> default
    | Some accept ->
        routes
        |> List.find_map (fun (content_type, route) ->
               if List.mem content_type accept then Some route else None)
        |> Option.value ~default
  in
  route ()

let parse_webfinger_address q =
  let re = Regex.e {|^@?([^@]+)(?:@([^@]+))?$|} in
  match Regex.match_group re q with
  | Ok [ _; username; domain ] ->
      let domain = if domain = "" then None else Some domain in
      (username, domain)
  | _ -> Httpq.Server.raise_error_response `Bad_request
