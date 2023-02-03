open Lwt.Infix

let get req =
  let access_token =
    let open Httpq.Server in
    match req |> query_opt "access_token" with
    | Some v -> v
    | None -> req |> header `Sec_websocket_protocol
  in
  let stream = req |> Httpq.Server.query "stream" in
  if stream <> "user" then Httpq.Server.raise_error_response `Bad_request;
  let stream = `User in

  let%lwt user_id =
    try%lwt
      Oauth.authenticate_access_token access_token >|= fun token ->
      Option.get token.resource_owner_id
    with _ -> Httpq.Server.raise_error_response `Unauthorized
  in

  Httpq.Server.websocket req @@ fun c ->
  let key = Streaming.make_key ~user_id ~stream in
  Streaming.(add key (`WebSocket c));
  let rec loop () =
    match%lwt Httpq.Server.ws_recv c with
    | None -> Lwt.return_unit (* Closed *)
    | Some _ -> loop () (* FIXME *)
  in
  Lwt.finalize loop (fun () ->
      Lwt.return @@ Streaming.remove key (`WebSocket c))
