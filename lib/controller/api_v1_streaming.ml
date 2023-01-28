open Lwt.Infix

let get req =
  let access_token = req |> Httpx.query "access_token" in
  let stream = req |> Httpx.query "stream" in
  if stream <> "user" then Http.raise_error_response `Bad_request;
  let stream = `User in

  let%lwt user_id =
    try%lwt
      Oauth.authenticate_access_token access_token >|= fun token ->
      Option.get token.resource_owner_id
    with _ -> Http.raise_error_response `Unauthorized
  in

  Httpx.websocket req @@ fun c ->
  let key = Streaming.make_key ~user_id ~stream in
  Streaming.(add key (`WebSocket c));
  let rec loop () =
    match%lwt Http.ws_recv c with
    | None -> Lwt.return_unit (* Closed *)
    | Some _ -> loop () (* FIXME *)
  in
  Lwt.finalize loop (fun () ->
      Lwt.return @@ Streaming.remove key (`WebSocket c))
