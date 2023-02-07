open Lwt.Infix

let content_type_app_xrd_xml =
  (`Content_type, "application/xrd+xml; charset=utf-8")

let content_type_app_jrd_json =
  (`Content_type, "application/jrd+json; charset=utf-8")

let content_type_app_json = (`Content_type, "application/json; charset=utf-8")

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
