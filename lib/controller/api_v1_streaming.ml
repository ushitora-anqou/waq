let get req =
  let access_token = req |> Httpx.query "access_token" in
  let stream = req |> Httpx.query "stream" in
  if stream <> "user" then Http.raise_error_response `Bad_request;

  try%lwt
    let%lwt token = Oauth.authenticate_access_token access_token in
    let user_id = Option.get token.resource_owner_id in
    Httpx.websocket req @@ fun c ->
    Streaming.add ~user_id ~stream (`WebSocket c);
    let rec loop () =
      match%lwt Http.ws_recv c with
      | None -> Lwt.return_unit (* Closed *)
      | Some _ -> loop () (* FIXME *)
    in
    loop ()
  with _ -> Http.raise_error_response `Unauthorized
