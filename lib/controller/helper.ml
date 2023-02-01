let content_type_app_xrd_xml =
  (`Content_type, "application/xrd+xml; charset=utf-8")

let content_type_app_jrd_json =
  (`Content_type, "application/jrd+json; charset=utf-8")

let content_type_app_json = (`Content_type, "application/json; charset=utf-8")

let authenticate_bearer = function
  | Http.Server.Request r -> (
      try
        let header = r.headers |> List.assoc `Authorization in
        assert (String.starts_with ~prefix:"Bearer " header);
        let bearer_token = String.sub header 7 (String.length header - 7) in
        Oauth.authenticate_access_token bearer_token
      with _ -> Http.Server.raise_error_response `Unauthorized)

let authenticate_user (r : Http.Server.request) =
  try
    let%lwt token = authenticate_bearer r in
    token.resource_owner_id |> Option.get |> Lwt.return
  with _ -> Http.Server.raise_error_response `Unauthorized

let int_of_string s =
  match int_of_string_opt s with
  | None -> Http.Server.raise_error_response `Bad_request
  | Some i -> i

let bool_of_string s =
  match bool_of_string_opt s with
  | None -> Http.Server.raise_error_response `Bad_request
  | Some b -> b
