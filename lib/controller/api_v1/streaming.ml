let may_subscribe ~user_id ~stream ~ws_conn subscription_ref =
  match !subscription_ref with
  | Some _ -> ()
  | None ->
      let key = Streaming.make_key ~user_id ~stream in
      let conn_id = Streaming.(add key (`WebSocket ws_conn)) in
      subscription_ref := Some (key, conn_id)

let may_unsubscribe subscription_ref =
  !subscription_ref
  |> Option.iter (fun (key, conn_id) -> Streaming.remove key conn_id);
  subscription_ref := None

let get _env req =
  let access_token =
    let open Yume.Server in
    req |> query_opt "access_token" |> function
    | Some v -> v
    | None -> req |> header `Sec_websocket_protocol
  in
  let stream = req |> Yume.Server.query_opt "stream" in

  let user_id =
    try
      Oauth_helper.authenticate_access_token access_token |> fun token ->
      Option.get token#resource_owner_id
    with _ -> Yume.Server.raise_error_response `Unauthorized
  in

  Yume.Server.websocket req @@ fun ws_conn ->
  let user_subscription_ref = ref None in
  let may_subscribe_user () =
    may_subscribe ~user_id ~stream:`User ~ws_conn user_subscription_ref
  in
  let may_unsubscribe_user () = may_unsubscribe user_subscription_ref in

  (match stream with
  | None -> ()
  | Some "user" -> may_subscribe_user ()
  | _ -> Yume.Server.raise_error_response `Bad_request);

  let rec loop () =
    match Yume.Server.ws_recv ws_conn with
    | None -> () (* Closed *)
    | Some json ->
        (match
           let ev = Yojson.Safe.from_string json |> Yojson.Safe.Util.to_assoc in
           let typ =
             ev |> List.assoc_opt "type"
             |> Option.map Yojson.Safe.Util.to_string
           in
           let stream =
             ev |> List.assoc_opt "stream"
             |> Option.map Yojson.Safe.Util.to_string
           in
           (typ, stream)
         with
        | Some "subscribe", Some "user" -> may_subscribe_user ()
        | Some "unsubscribe", Some "user" -> may_unsubscribe_user ()
        | _ | (exception (Yojson.Json_error _ | Yojson.Safe.Util.Type_error _))
          ->
            Logs.warn (fun m -> m "Unhandled websocket event: %s" json);
            ());
        loop ()
  in
  Fun.protect ~finally:may_unsubscribe_user loop
