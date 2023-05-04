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
let raise_error_response = Httpq.Server.raise_error_response

let authenticate_bearer = function
  | Httpq.Server.Request r -> (
      try
        let header = r.headers |> List.assoc `Authorization in
        assert (String.starts_with ~prefix:"Bearer " header);
        let bearer_token = String.sub header 7 (String.length header - 7) in
        Oauth_helper.authenticate_access_token bearer_token
      with _ -> raise_error_response `Unauthorized)

let authenticate_user (r : Httpq.Server.request) : Model.User.t Lwt.t =
  try
    let%lwt token = authenticate_bearer r in
    Db.(e User.(get_one ~id:(Option.get token#resource_owner_id)))
  with _ -> raise_error_response `Unauthorized

let authenticate_account (r : Httpq.Server.request) : Model.Account.t Lwt.t =
  try
    let%lwt user = authenticate_user r in
    Db.(e Account.(get_one ~id:user#account_id))
  with _ -> raise_error_response `Unauthorized

let may_authenticate_user r =
  try%lwt authenticate_user r >|= Option.some with _ -> Lwt.return_none

let may_authenticate_account r =
  try%lwt authenticate_account r >|= Option.some with _ -> Lwt.return_none

let int_of_string s =
  match int_of_string_opt s with
  | None -> raise_error_response `Bad_request
  | Some i -> i

let bool_of_string s =
  match bool_of_string_opt s with
  | None -> raise_error_response `Bad_request
  | Some b -> b

let respond_yojson ?(headers = []) y =
  Yojson.Safe.to_string y
  |> Httpq.Server.respond ~headers:(content_type_app_json :: headers)

let respond_activity_yojson y =
  Yojson.Safe.to_string y
  |> Httpq.Server.respond ~headers:[ content_type_app_activity_json ]

let render ~default routes req =
  let accept =
    Httpq.Server.header_opt `Accept req
    |> Option.map (String.split_on_char ',' |.> List.map String.trim)
  in
  let default = ("default", default) in
  let content_type, route =
    match accept with
    | None -> default
    | Some accept ->
        routes
        |> List.find_map (fun (content_type, route) ->
               if List.mem content_type accept then Some (content_type, route)
               else None)
        |> Option.value ~default
  in
  Logq.debug (fun m -> m "[render] Choose %s" content_type);
  route ()

let parse_webfinger_address q =
  let re = Regex.e {|^@?([^@]+)(?:@([^@]+))?$|} in
  match Regex.match_ re q with
  | [ [| _; Some username; domain |] ] ->
      (username.substr, domain |> Option.map (fun (x : Regex.group) -> x.substr))
  | _ -> raise_error_response `Bad_request

let raise_if_no_row_found ?(status = `Bad_request) f =
  try%lwt f with Sqlx.Error.NoRowFound -> raise_error_response status

let string_to_status_id s = s |> int_of_string |> Model.Status.ID.of_int
let string_to_account_id s = s |> int_of_string |> Model.Account.ID.of_int

let omit_html_tags =
  let r = Regex.e {|<[^>]*>|} in
  Regex.replace r (fun _ -> "")

let expect_assoc = function
  | `Assoc l -> l
  | _ -> raise_error_response `Bad_request
