include Lwt.Infix
module Uri = Httpq.Uri
module Ptime = Waq.Util.Ptime

let ( |.> ) f g a = a |> f |> g
let ignore_lwt = Waq.Util.ignore_lwt
let fetch = Httpq.Client.fetch
let fetch_exn = Httpq.Client.fetch_exn
let ( ^/ ) a b = a ^ "/" ^ b

let expect_string = function
  | `String s -> s
  | _ -> failwith "Expected string, got something different"

let expect_assoc = function
  | `Assoc l -> l
  | _ -> failwith "Expected assoc, got something different"

let expect_list = function
  | `List l -> l
  | _ -> failwith "Expected list, got something different"

let expect_bool = function
  | `Bool b -> b
  | _ -> failwith "Expected bool, got something different"

let expect_int = function
  | `Int v -> v
  | _ -> failwith "Expected int, got something different"

let with_lock mtx f =
  match mtx with None -> f () | Some mtx -> Lwt_mutex.with_lock mtx f

let new_session f =
  let path =
    Sys.getenv_opt "WAQ_BIN" |> Option.value ~default:"test_e2e/launch_waq.sh"
  in
  let open Unix in
  let ic = open_process_args_in path [| path |] in
  let token1 = In_channel.input_line ic |> Option.value ~default:"" in
  let _token2 = In_channel.input_line ic |> Option.value ~default:"" in
  let _token3 = In_channel.input_line ic |> Option.value ~default:"" in
  let pid = process_in_pid ic in
  Fun.protect
    (fun () -> f token1)
    ~finally:(fun () ->
      kill pid Sys.sigint;
      close_process_in ic |> ignore)

let new_mastodon_session f =
  let path =
    Sys.getenv_opt "MSTDN_BIN"
    |> Option.value ~default:"test_e2e/launch_mstdn.sh"
  in
  let open Unix in
  let ic = open_process_args_in path [| path |] in
  let token = In_channel.input_line ic |> Option.value ~default:"" in
  let pid = process_in_pid ic in
  Fun.protect
    (fun () -> f token)
    ~finally:(fun () ->
      Logq.debug (fun m -> m "Killing mastodon processes");
      kill pid Sys.sigint;
      close_process_in ic |> ignore)

let make_waq_and_mstdn_scenario ?(timeout = 30.0) handler () : unit =
  new_session @@ fun waq_token ->
  Logq.debug (fun m -> m "Access token for Waq: %s" waq_token);
  new_mastodon_session @@ fun mstdn_token ->
  Logq.debug (fun m -> m "Access token for Mastodon: %s" mstdn_token);
  Unix.sleep 10;
  Lwt_main.run
  @@ Lwt.pick
       [
         handler waq_token mstdn_token;
         (Lwt_unix.sleep timeout >>= fun () -> failwith "Timeout");
       ]

let make_waq_scenario ?(timeout = 30.0) handler () : unit =
  new_session @@ fun waq_token ->
  Logq.debug (fun m -> m "Access token for Waq: %s" waq_token);
  Unix.sleep 1;
  Lwt_main.run
  @@ Lwt.pick
       [
         handler waq_token;
         (Lwt_unix.sleep timeout >>= fun () -> failwith "Timeout");
       ]

let waq_server_name = Sys.getenv "WAQ_SERVER_NAME"
let waq_server_domain = Uri.(of_string waq_server_name |> domain)
let waq url = waq_server_name ^ url

let mstdn url =
  let server_name = "http://localhost:3000" in
  server_name ^ url

let url = function `Waq -> waq | `Mstdn -> mstdn

let pp_json (s : string) =
  Logq.debug (fun m -> m "%s" Yojson.Safe.(from_string s |> pretty_to_string))
  [@@warning "-32"]

let do_fetch ?token ?(meth = `GET) ?(body = "") kind target =
  let headers = [ (`Accept, "application/json") ] in
  let headers =
    match meth with
    | `POST -> (`Content_type, "application/json") :: headers
    | _ -> headers
  in
  let headers =
    match token with
    | Some token -> (`Authorization, "Bearer " ^ token) :: headers
    | None -> headers
  in
  fetch_exn ~headers ~meth ~body (url kind target)

type account = {
  id : string;
  username : string;
  acct : string;
  display_name : string;
  last_status_at : string option;
  statuses_count : int;
  followers_count : int;
  following_count : int;
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

type media_attachment = { id : string; type_ : string [@key "type"] }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type status_mention = {
  id : string;
  username : string;
  url : string;
  acct : string;
}
[@@deriving make, yojson]

type status = {
  id : string;
  uri : string;
  reblog : status option;
  reblogged : bool;
  reblogs_count : int;
  favourited : bool;
  account : account;
  favourites_count : int;
  media_attachments : media_attachment list;
  spoiler_text : string;
  mentions : status_mention list;
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

type relationship = { id : string; following : bool; followed_by : bool }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type notification = {
  id : string;
  typ : string; [@key "type"]
  created_at : string;
  account : account;
  status : status option; [@yojson.option]
}
[@@deriving make, yojson]

type marker = { last_read_id : string; version : int; updated_at : string }
[@@deriving make, yojson]

let update_credentials ~token kind ?display_name () =
  let target = "/api/v1/accounts/update_credentials" in
  let headers =
    [
      (`Accept, "application/json");
      (`Authorization, "Bearer " ^ token);
      ( `Content_type,
        "multipart/form-data; \
         boundary=---------------------------91791948726096252761377705945" );
    ]
  in
  let body =
    [ {|-----------------------------91791948726096252761377705945--|}; {||} ]
  in
  let body =
    match display_name with
    | None -> body
    | Some display_name ->
        [
          {|-----------------------------91791948726096252761377705945|};
          {|Content-Disposition: form-data; name="display_name"|};
          {||};
          display_name;
        ]
        @ body
  in
  assert (List.length body <> 2);
  let body = String.concat "\r\n" body in
  fetch_exn ~headers ~meth:`PATCH ~body (url kind target)
  >|= Yojson.Safe.from_string >|= account_of_yojson

let lookup_via_v1_accounts_lookup ~token kind ?domain ~username () =
  let target =
    let src = "/api/v1/accounts/lookup?acct=" in
    match domain with
    | None -> src ^ username
    | Some domain -> src ^ username ^ "@" ^ domain
  in
  let%lwt r = do_fetch ~token kind target in
  let a = r |> Yojson.Safe.from_string |> account_of_yojson in
  Lwt.return (a.id, a.username, a.acct)

let lookup_via_v1_accounts_search ?token kind ?domain ~username () =
  let target =
    let src = "/api/v1/accounts/search?resolve=true&q=@" in
    match domain with
    | None -> src ^ username
    | Some domain -> src ^ username ^ "@" ^ domain
  in
  let%lwt r = do_fetch ?token kind target in
  let l =
    match Yojson.Safe.from_string r with
    | `List [ `Assoc l ] -> l
    | _ -> assert false
  in
  Lwt.return
    ( l |> List.assoc "id" |> expect_string,
      l |> List.assoc "username" |> expect_string,
      l |> List.assoc "acct" |> expect_string )

let search ?token kind q =
  let queries = [ ("resolve", [ "true" ]); ("q", [ q ]) ] in
  let u = Uri.of_string "/api/v2/search" in
  let u = Uri.add_query_params u queries in
  let%lwt r = do_fetch ?token kind (Uri.to_string u) in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  Lwt.return
    ( List.assoc "accounts" l |> expect_list |> List.map account_of_yojson,
      List.assoc "statuses" l |> expect_list |> List.map status_of_yojson,
      List.assoc "hashtags" l |> expect_list )

let lookup ~token kind ?domain ~username () =
  search ~token kind
    (domain
    |> Option.fold ~none:("@" ^ username) ~some:(fun domain ->
           "@" ^ username ^ "@" ^ domain))
  >|= function
  | [ acct ], _, _ -> (acct.id, acct.username, acct.acct)
  | _ -> assert false

let get_account kind id =
  do_fetch kind ("/api/v1/accounts/" ^ id)
  >|= Yojson.Safe.from_string >|= account_of_yojson

let get_relationships ~token kind account_ids =
  let target =
    "/api/v1/accounts/relationships?"
    ^ (account_ids |> List.map (fun id -> "id[]=" ^ id) |> String.concat "&")
  in
  do_fetch ~token kind target
  >|= Yojson.Safe.from_string >|= expect_list
  >|= List.map relationship_of_yojson

let get_followers ?token kind account_id =
  do_fetch ?token kind ("/api/v1/accounts/" ^ account_id ^ "/followers")
  >|= Yojson.Safe.from_string >|= expect_list >|= List.map account_of_yojson

let get_following ?token kind account_id =
  do_fetch ?token kind ("/api/v1/accounts/" ^ account_id ^ "/following")
  >|= Yojson.Safe.from_string >|= expect_list >|= List.map account_of_yojson

let get_notifications ?token kind =
  do_fetch ?token kind "/api/v1/notifications"
  >|= Yojson.Safe.from_string >|= expect_list
  >|= List.map notification_of_yojson

let markers_of_yojson j =
  let l = expect_assoc j in
  ( List.assoc_opt "home" l |> Option.map marker_of_yojson,
    List.assoc_opt "notifications" l |> Option.map marker_of_yojson )

let get_markers ?token kind timelines =
  do_fetch ?token kind
    ("/api/v1/markers?"
    ^ (timelines |> List.map (fun s -> "timeline[]=" ^ s) |> String.concat "&")
    )
  >|= Yojson.Safe.from_string >|= markers_of_yojson

let post_markers ?token kind values =
  let body =
    values
    |> List.map (fun (timeline, last_read_id) ->
           (timeline, `Assoc [ ("last_read_id", `String last_read_id) ]))
    |> fun l -> `Assoc l |> Yojson.Safe.to_string
  in
  do_fetch ~meth:`POST ?token kind ~body "/api/v1/markers"
  >|= Yojson.Safe.from_string >|= markers_of_yojson

let follow ~token kind account_id =
  let%lwt r =
    do_fetch ~meth:`POST ~token kind
      ("/api/v1/accounts/" ^ account_id ^ "/follow")
  in
  assert (
    Yojson.Safe.from_string r |> expect_assoc |> List.assoc "following"
    |> expect_bool);
  Lwt.return_unit

let unfollow ~token kind account_id =
  do_fetch ~meth:`POST ~token kind
    ("/api/v1/accounts/" ^ account_id ^ "/unfollow")
  |> ignore_lwt

let get_status kind ?token status_id =
  do_fetch ?token kind ("/api/v1/statuses/" ^ status_id)
  >|= Yojson.Safe.from_string >|= status_of_yojson

let get_account_statuses kind ?token ?(exclude_replies = false) account_id =
  do_fetch ?token kind
    ("/api/v1/accounts/" ^ account_id ^ "/statuses?exclude_replies="
    ^ string_of_bool exclude_replies)
  >|= Yojson.Safe.from_string >|= expect_list >|= List.map status_of_yojson

let get_status_context kind status_id =
  let%lwt r = do_fetch kind ("/api/v1/statuses/" ^ status_id ^ "/context") in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  match l with
  | [ ("ancestors", `List ancestors); ("descendants", `List descendants) ]
  | [ ("descendants", `List descendants); ("ancestors", `List ancestors) ] ->
      let ancestors = ancestors |> List.map status_of_yojson in
      let descendants = descendants |> List.map status_of_yojson in
      Lwt.return (ancestors, descendants)
  | _ -> assert false

let post ~token kind ?spoiler_text ?content ?in_reply_to_id ?(media_ids = []) ()
    =
  let content = content |> Option.value ~default:"こんにちは、世界！" in
  let body =
    let l =
      [
        ("status", `String content);
        ("media_ids", `List (media_ids |> List.map (fun s -> `String s)));
      ]
    in
    let l =
      in_reply_to_id
      |> Option.fold ~none:l ~some:(fun id ->
             ("in_reply_to_id", `String id) :: l)
    in
    let l =
      spoiler_text
      |> Option.fold ~none:l ~some:(fun s -> ("spoiler_text", `String s) :: l)
    in
    `Assoc l |> Yojson.Safe.to_string
  in
  do_fetch ~token ~meth:`POST ~body kind "/api/v1/statuses"
  >|= Yojson.Safe.from_string >|= status_of_yojson

let delete_status ~token kind status_id =
  do_fetch ~token ~meth:`DELETE kind ("/api/v1/statuses/" ^ status_id)
  >|= Yojson.Safe.from_string >|= status_of_yojson

let reblog ~token kind ~id =
  do_fetch ~token ~meth:`POST kind ("/api/v1/statuses/" ^ id ^ "/reblog")
  >|= Yojson.Safe.from_string >|= status_of_yojson

let unreblog ~token kind ~id =
  do_fetch ~token ~meth:`POST kind ("/api/v1/statuses/" ^ id ^ "/unreblog")
  >|= Yojson.Safe.from_string >|= status_of_yojson

let fav ~token kind ~id =
  do_fetch ~token ~meth:`POST kind ("/api/v1/statuses/" ^ id ^ "/favourite")
  >|= Yojson.Safe.from_string >|= status_of_yojson

let unfav ~token kind ~id =
  do_fetch ~token ~meth:`POST kind ("/api/v1/statuses/" ^ id ^ "/unfavourite")
  >|= Yojson.Safe.from_string >|= status_of_yojson

let get_favourited_by ~token kind ~id =
  do_fetch ~token ~meth:`GET kind ("/api/v1/statuses/" ^ id ^ "/favourited_by")
  >|= Yojson.Safe.from_string >|= expect_list >|= List.map account_of_yojson

let home_timeline ~token kind =
  do_fetch ~token kind "/api/v1/timelines/home" >|= fun r ->
  match Yojson.Safe.from_string r with `List l -> l | _ -> assert false

let fetch_access_token ~username =
  let%lwt r =
    fetch_exn ~meth:`POST
      ~headers:[ (`Content_type, "application/json") ]
      ~body:
        {|{"client_name":"foo","redirect_uris":"http://example.com?origin=http://example.com"}|}
      (waq "/api/v1/apps")
  in
  let client_id, client_secret =
    match Yojson.Safe.from_string r with
    | `Assoc l ->
        ( List.assoc "client_id" l |> expect_string,
          List.assoc "client_secret" l |> expect_string )
    | _ -> assert false
  in

  let%lwt r =
    let body =
      Uri.encoded_of_query
        [
          ("response_type", [ "code" ]);
          ("client_id", [ client_id ]);
          ("redirect_uri", [ "http://example.com?origin=http://example.com" ]);
          ("username", [ username ]);
          ("password", [ username ^ "password" ]);
        ]
    in
    fetch ~meth:`POST ~body (waq "/oauth/authorize")
  in
  let auth_code =
    match r with
    | Ok (`Found, headers, _body) ->
        (* 0123456789012345678901234
           http://example.com?code=... *)
        headers |> List.assoc "location" |> Uri.of_string |> Uri.query
        |> List.assoc "code" |> List.hd
    | _ -> assert false
  in

  let%lwt r =
    fetch_exn ~meth:`POST
      ~headers:[ (`Content_type, "application/json") ]
      ~body:
        (`Assoc
           [
             ("grant_type", `String "authorization_code");
             ("code", `String auth_code);
             ("client_id", `String client_id);
             ("client_secret", `String client_secret);
             ( "redirect_uri",
               `String "http://example.com?origin=http://example.com" );
           ]
        |> Yojson.Safe.to_string)
      (waq "/oauth/token")
  in
  match Yojson.Safe.from_string r with
  | `Assoc l -> l |> List.assoc "access_token" |> expect_string |> Lwt.return
  | _ -> assert false

let websocket ?mtx ~token kind ?target handler f =
  let open Websocket_lwt_unix in
  let target =
    match target with
    | Some target -> target
    | None -> "/api/v1/streaming?stream=user"
  in
  let uri = Uri.of_string (url kind target) in
  let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let%lwt client = Conduit_lwt_unix.endp_to_client ~ctx endp in

  let extra_headers =
    Cohttp.Header.of_list [ ("Sec-WebSocket-Protocol", token) ]
  in
  let%lwt conn = connect ~extra_headers ~ctx client uri in
  let close_sent = ref false in
  let pushf msg =
    match msg with
    | Some content -> write conn (Websocket.Frame.create ~content ())
    | None when !close_sent -> Lwt.return_unit
    | None ->
        write conn (Websocket.Frame.create ~opcode:Close ());%lwt
        Lwt.return (close_sent := true)
  in
  let rec react () =
    match%lwt read conn with
    | { Websocket.Frame.opcode = Ping; _ } ->
        write conn (Websocket.Frame.create ~opcode:Pong ());%lwt
        react ()
    | { opcode = Pong; _ } -> react ()
    | { opcode = Text; content; _ } | { opcode = Binary; content; _ } ->
        with_lock mtx (fun () -> handler content pushf);%lwt
        react ()
    | { opcode = Close; content; _ } ->
        if !close_sent then Lwt.return_unit
        else if String.length content >= 2 then
          write conn
            (Websocket.Frame.create ~opcode:Close
               ~content:(String.sub content 0 2) ())
        else write conn (Websocket.Frame.close 1000);%lwt
        close_transport conn
    | _ -> close_transport conn
  in
  Lwt.join [ with_lock mtx (fun () -> f pushf); react () ]

let websocket_handler_state_machine ~states ~init () =
  let current = ref init in
  let set_current v = current := v in
  let handler content pushf =
    let real_handler = states |> List.assoc !current in
    let%lwt next_state =
      real_handler (content |> Yojson.Safe.from_string |> expect_assoc) pushf
    in
    set_current next_state;
    Lwt.return_unit
  in
  (set_current, handler)

let websocket_stack kind ~token ?num_msgs f =
  let recv_msgs = ref [] in
  let handler content pushf =
    recv_msgs := content :: !recv_msgs;
    match num_msgs with
    | Some num_msgs when List.length !recv_msgs = num_msgs -> pushf None
    | _ -> Lwt.return_unit
  in
  websocket kind ~token handler (fun pushf ->
      f pushf;%lwt
      match num_msgs with None -> pushf None | Some _ -> Lwt.return_unit)
  >|= fun () -> !recv_msgs

let expect_exc_lwt f =
  (try%lwt
     let%lwt _ = f () in
     Lwt.return_false
   with _ -> Lwt.return_true)
  >|= fun b -> assert b
