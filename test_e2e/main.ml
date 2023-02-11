open Lwt.Infix
module Uri = Httpq.Uri
module Ptime = Waq.Util.Ptime

let ( |.> ) f g a = a |> f |> g
let ignore_lwt = Waq.Util.ignore_lwt
let fetch = Httpq.Client.fetch
let fetch_exn = Httpq.Client.fetch_exn

let expect_string = function
  | `String s -> s
  | _ -> failwith "Expected string, got something different"

let expect_assoc = function
  | `Assoc l -> l
  | _ -> failwith "Expected assoc, got something different"

let expect_list = function
  | `List l -> l
  | _ -> failwith "Expected list, got something different"

let with_lock mtx f =
  match mtx with None -> f () | Some mtx -> Lwt_mutex.with_lock mtx f

let new_session f =
  let path =
    Sys.getenv_opt "WAQ_BIN" |> Option.value ~default:"test_e2e/launch_waq.sh"
  in
  let open Unix in
  let ic = open_process_args_in path [| path |] in
  let token = In_channel.input_line ic |> Option.value ~default:"" in
  let pid = process_in_pid ic in
  Fun.protect
    (fun () -> f token)
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

let make_waq_and_mstdn_scenario handler () : unit =
  new_session @@ fun waq_token ->
  Logq.debug (fun m -> m "Access token for Waq: %s" waq_token);
  new_mastodon_session @@ fun mstdn_token ->
  Logq.debug (fun m -> m "Access token for Mastodon: %s" mstdn_token);
  Unix.sleep 10;
  Lwt_main.run @@ handler waq_token mstdn_token

let make_waq_scenario handler () : unit =
  new_session @@ fun waq_token ->
  Logq.debug (fun m -> m "Access token for Waq: %s" waq_token);
  Unix.sleep 1;
  Lwt_main.run @@ handler waq_token

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

let lookup_via_v1_accounts_search ~token kind ?domain ~username () =
  let target =
    let src = "/api/v1/accounts/search?resolve=true&q=@" in
    match domain with
    | None -> src ^ username
    | Some domain -> src ^ username ^ "@" ^ domain
  in
  let%lwt r = do_fetch ~token kind target in
  let l =
    match Yojson.Safe.from_string r with
    | `List [ `Assoc l ] -> l
    | _ -> assert false
  in
  Lwt.return
    ( l |> List.assoc "id" |> expect_string,
      l |> List.assoc "username" |> expect_string,
      l |> List.assoc "acct" |> expect_string )

let lookup ~token kind ?domain ~username () =
  let target =
    let src = "/api/v2/search?resolve=true&q=@" in
    match domain with
    | None -> src ^ username
    | Some domain -> src ^ username ^ "@" ^ domain
  in
  let%lwt r = do_fetch ~token kind target in
  let l =
    match Yojson.Safe.from_string r with
    | `Assoc l -> (
        match l |> List.assoc "accounts" |> expect_list with
        | [ `Assoc l ] -> l
        | _ -> assert false)
    | _ -> assert false
  in
  Lwt.return
    ( l |> List.assoc "id" |> expect_string,
      l |> List.assoc "username" |> expect_string,
      l |> List.assoc "acct" |> expect_string )

let follow ~token kind account_id =
  do_fetch ~meth:`POST ~token kind ("/api/v1/accounts/" ^ account_id ^ "/follow")
  |> ignore_lwt

let unfollow ~token kind account_id =
  do_fetch ~meth:`POST ~token kind
    ("/api/v1/accounts/" ^ account_id ^ "/unfollow")
  |> ignore_lwt

type account = { id : string } [@@deriving yojson { strict = false }]

type status = {
  id : string;
  uri : string;
  reblog : status option;
  reblogged : bool;
  reblogs_count : int;
  favourited : bool;
  account : account;
}
[@@deriving yojson { strict = false }]

let get_status kind ?token status_id =
  do_fetch ?token kind ("/api/v1/statuses/" ^ status_id)
  >|= Yojson.Safe.from_string >|= status_of_yojson >|= Result.get_ok

let get_status_context kind status_id =
  let%lwt r = do_fetch kind ("/api/v1/statuses/" ^ status_id ^ "/context") in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  match l with
  | [ ("ancestors", `List ancestors); ("descendants", `List descendants) ]
  | [ ("descendants", `List descendants); ("ancestors", `List ancestors) ] ->
      let ancestors =
        ancestors |> List.map (status_of_yojson |.> Result.get_ok)
      in
      let descendants =
        descendants |> List.map (status_of_yojson |.> Result.get_ok)
      in
      Lwt.return (ancestors, descendants)
  | _ -> assert false

let post ~token kind ?content ?in_reply_to_id () =
  let content = content |> Option.value ~default:"こんにちは、世界！" in
  let body =
    let l = [ ("status", `String content) ] in
    let l =
      in_reply_to_id
      |> Option.fold ~none:l ~some:(fun id ->
             ("in_reply_to_id", `String id) :: l)
    in
    `Assoc l |> Yojson.Safe.to_string
  in
  do_fetch ~token ~meth:`POST ~body kind "/api/v1/statuses"
  >|= Yojson.Safe.from_string >|= status_of_yojson >|= Result.get_ok

let reblog ~token kind ~id =
  do_fetch ~token ~meth:`POST kind ("/api/v1/statuses/" ^ id ^ "/reblog")
  >|= Yojson.Safe.from_string >|= status_of_yojson >|= Result.get_ok

let fav ~token kind ~id =
  do_fetch ~token ~meth:`POST kind ("/api/v1/statuses/" ^ id ^ "/favourite")
  >|= Yojson.Safe.from_string >|= status_of_yojson >|= Result.get_ok

let unfav ~token kind ~id =
  do_fetch ~token ~meth:`POST kind ("/api/v1/statuses/" ^ id ^ "/unfavourite")
  >|= Yojson.Safe.from_string >|= status_of_yojson >|= Result.get_ok

let get_favourited_by ~token kind ~id =
  do_fetch ~token ~meth:`GET kind ("/api/v1/statuses/" ^ id ^ "/favourited_by")
  >|= Yojson.Safe.from_string >|= expect_list
  >|= List.map (account_of_yojson |.> Result.get_ok)

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
    | None ->
        Printf.sprintf "/api/v1/streaming?access_token=%s&stream=user" token
  in
  let uri = Uri.of_string (url kind target) in
  let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let%lwt client = Conduit_lwt_unix.endp_to_client ~ctx endp in
  let%lwt conn = connect ~ctx client uri in
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

let waq_mstdn_scenario_1 =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Connect WebSocket *)
  let ws_statuses = ref [] in
  let _set_current_state, handler =
    websocket_handler_state_machine ~init:`Recv
      ~states:
        [
          ( `Recv,
            fun l _pushf ->
              assert (List.assoc "stream" l = `List [ `String "user" ]);
              assert (List.assoc "event" l |> expect_string = "update");
              let payload = List.assoc "payload" l |> expect_string in
              ws_statuses :=
                (Yojson.Safe.from_string payload |> expect_assoc)
                :: !ws_statuses;
              Lwt.return `Recv );
        ]
      ()
  in
  let uris = ref [] in
  websocket `Waq ~token:waq_token handler (fun pushf ->
      (* Lookup @admin@localhost:3000 *)
      let%lwt admin_id, username, acct =
        lookup `Waq ~token:waq_token ~username:"admin" ~domain:"localhost:3000"
          ()
      in
      assert (username = "admin");
      assert (acct = "admin@localhost:3000");

      (* Follow @admin@localhost:3000 *)
      follow `Waq ~token:waq_token admin_id;%lwt
      Lwt_unix.sleep 1.0;%lwt

      (* Post by @admin@localhost:3000 *)
      let%lwt { uri; _ } = post `Mstdn ~token:mstdn_token () in
      uris := uri :: !uris;
      Lwt_unix.sleep 1.0;%lwt

      (* Post by me *)
      let%lwt { uri = uri2; _ } = post `Waq ~token:waq_token () in
      uris := uri2 :: !uris;
      Lwt_unix.sleep 1.0;%lwt

      (* Get my home timeline and check *)
      (home_timeline `Waq ~token:waq_token >|= function
       | [ `Assoc l2; `Assoc l ] ->
           (* Check if the timeline is correct *)
           assert (uri = (l |> List.assoc "uri" |> expect_string));
           assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
           ()
       | _ -> assert false);%lwt

      (* Unfollow @admin@localhost:3000 *)
      unfollow `Waq ~token:waq_token admin_id;%lwt
      Lwt_unix.sleep 1.0;%lwt

      (* Get my home timeline and check again *)
      (home_timeline `Waq ~token:waq_token >|= function
       | [ `Assoc l2 ] ->
           (* Check if the timeline is correct *)
           assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
           ()
       | _ -> assert false);%lwt

      pushf None);%lwt

  let expected_uris = List.sort compare !uris in
  let got_uris =
    !ws_statuses
    |> List.map (fun s -> s |> List.assoc "uri" |> expect_string)
    |> List.sort compare
  in
  assert (expected_uris = got_uris);

  Lwt.return_unit

let waq_mstdn_scenario_2 =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup me from localhost:3000 *)
  let%lwt aid, _, _ =
    lookup `Mstdn ~token:mstdn_token ~username:"user1" ~domain:waq_server_domain
      ()
  in

  (* Follow me from @admin@localhost:3000 *)
  follow `Mstdn ~token:mstdn_token aid;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by @admin@localhost:3000 *)
  let%lwt { uri; _ } = post `Mstdn ~token:mstdn_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Post by me *)
  let%lwt { uri = uri2; _ } = post `Waq ~token:waq_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 and check *)
  (home_timeline `Mstdn ~token:mstdn_token >|= function
   | [ `Assoc l2; `Assoc l ] ->
       (* Check if the timeline is correct *)
       assert (uri = (List.assoc "uri" l |> expect_string));
       assert (uri2 = (List.assoc "uri" l2 |> expect_string));
       ()
   | _ -> assert false);%lwt

  (* Unfollow me from @admin@localhost:3000 *)
  unfollow `Mstdn ~token:mstdn_token aid;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 and check again *)
  (home_timeline `Mstdn ~token:mstdn_token >|= function
   | [ `Assoc l ] ->
       (* Check if the timeline is correct *)
       assert (uri = (List.assoc "uri" l |> expect_string));
       ()
   | _ -> assert false);%lwt

  Lwt.return_unit

let waq_mstdn_scenario_3 =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup me from localhost:3000 *)
  let%lwt aid, _, _ =
    lookup `Mstdn ~token:mstdn_token ~username:"user1" ~domain:waq_server_domain
      ()
  in

  (* Lookup @admin@localhost:3000 *)
  let%lwt admin_id, _username, _acct =
    lookup `Waq ~token:waq_token ~username:"admin" ~domain:"localhost:3000" ()
  in

  (* Follow @admin@localhost:3000 *)
  follow `Waq ~token:waq_token admin_id;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Follow me from @admin@localhost:3000 *)
  follow `Mstdn ~token:mstdn_token aid;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by me *)
  let%lwt { uri; _ } = post `Waq ~token:waq_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 and obtain the status's id *)
  let%lwt id =
    home_timeline `Mstdn ~token:mstdn_token >|= function
    | [ `Assoc l ] ->
        (* Check if the timeline is correct *)
        assert (uri = (List.assoc "uri" l |> expect_string));
        List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  (* Reply by @admin@localhost:3000 *)
  let%lwt { uri; _ } = post `Mstdn ~token:mstdn_token ~in_reply_to_id:id () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of me and obtain the reply's id *)
  let%lwt id =
    home_timeline `Waq ~token:waq_token >|= function
    | [ `Assoc l; `Assoc l2 ] ->
        assert (uri = (List.assoc "uri" l |> expect_string));
        assert (List.assoc "id" l2 = List.assoc "in_reply_to_id" l);
        assert (
          List.assoc "account" l2 |> expect_assoc |> List.assoc "id"
          = List.assoc "in_reply_to_account_id" l);
        List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  (* Reply by me *)
  let%lwt { uri; _ } = post `Waq ~token:waq_token ~in_reply_to_id:id () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 and check *)
  let%lwt _ =
    home_timeline `Mstdn ~token:mstdn_token >|= function
    | [ `Assoc l; `Assoc l2; `Assoc _ ] ->
        (* Check if the timeline is correct *)
        assert (uri = (List.assoc "uri" l |> expect_string));
        assert (List.assoc "id" l2 = List.assoc "in_reply_to_id" l);
        assert (
          List.assoc "account" l2 |> expect_assoc |> List.assoc "id"
          = List.assoc "in_reply_to_account_id" l);
        List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  Lwt.return_unit

let waq_mstdn_scenario_4 =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup me from localhost:3000 *)
  let%lwt aid, _, _ =
    lookup `Mstdn ~token:mstdn_token ~username:"user1" ~domain:waq_server_domain
      ()
  in

  (* Follow me from @admin@localhost:3000 *)
  follow `Mstdn ~token:mstdn_token aid;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by user2 *)
  let%lwt waq_token' = fetch_access_token ~username:"user2" in
  let%lwt { id; uri; _ } = post `Waq ~token:waq_token' () in

  (* Reblog by me (user1) *)
  let%lwt _ = reblog `Waq ~token:waq_token ~id in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 *)
  let%lwt _ =
    home_timeline `Mstdn ~token:mstdn_token >|= function
    | [ `Assoc l ] ->
        (* Check if the timeline is correct *)
        let reblog_uri =
          l |> List.assoc "reblog" |> expect_assoc |> List.assoc "uri"
          |> expect_string
        in
        assert (uri = reblog_uri)
    | _ -> assert false
  in

  Lwt.return_unit

let waq_mstdn_scenario_5 =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup @admin@localhost:3000 *)
  let%lwt admin_id, _username, _acct =
    lookup `Waq ~token:waq_token ~username:"admin" ~domain:"localhost:3000" ()
  in

  (* Follow @admin@localhost:3000 *)
  follow `Waq ~token:waq_token admin_id;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post and reblog by @admin@localhost:3000 *)
  let%lwt { id; uri; _ } = post `Mstdn ~token:mstdn_token () in
  Lwt_unix.sleep 1.0;%lwt
  let%lwt _ = reblog `Mstdn ~token:mstdn_token ~id in
  Lwt_unix.sleep 1.0;%lwt

  (* Get my home timeline and check *)
  (home_timeline `Waq ~token:waq_token >|= function
   | [ `Assoc l1; `Assoc l2 ] ->
       (* Check if the timeline is correct *)
       assert (uri = (l2 |> List.assoc "uri" |> expect_string));
       assert (
         uri
         = (l1 |> List.assoc "reblog" |> expect_assoc |> List.assoc "uri"
          |> expect_string));
       ()
   | _ -> assert false);%lwt

  Lwt.return_unit

let waq_mstdn_scenario_6_fav =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup @admin@localhost:3000 *)
  let%lwt admin_id, _username, _acct =
    lookup `Waq ~token:waq_token ~username:"admin" ~domain:"localhost:3000" ()
  in
  (* Follow @admin@localhost:3000 *)
  follow `Waq ~token:waq_token admin_id;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Get user1's id on localhost:3000 *)
  let%lwt mstdn_user1_id, _, _ =
    lookup `Mstdn ~token:mstdn_token ~username:"user1" ()
  in

  (* Post by @admin@localhost:3000 *)
  let%lwt { id = mstdn_post_id; _ } = post `Mstdn ~token:mstdn_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get id of the post *)
  let%lwt id =
    home_timeline `Waq ~token:waq_token >|= function
    | [ `Assoc l ] -> List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  (* Favourite the post by me *)
  let%lwt s = fav `Waq ~token:waq_token ~id in
  Lwt_unix.sleep 1.0;%lwt
  assert s.favourited;
  let%lwt s = get_status `Waq ~token:waq_token id in
  assert s.favourited;

  (* Check if the post is favourited in localhost:3000 *)
  (match%lwt get_favourited_by `Mstdn ~token:mstdn_token ~id:mstdn_post_id with
  | [ a ] ->
      assert (a.id = mstdn_user1_id);
      Lwt.return_unit
  | _ -> assert false);%lwt

  (* Unfavourite the post *)
  let%lwt s = unfav `Waq ~token:waq_token ~id in
  Lwt_unix.sleep 1.0;%lwt
  assert (not s.favourited);
  let%lwt s = get_status `Waq ~token:waq_token id in
  assert (not s.favourited);

  (* Check if the post is unfavourited *)
  (match%lwt get_favourited_by `Mstdn ~token:mstdn_token ~id:mstdn_post_id with
  | [] -> Lwt.return_unit
  | _ -> assert false);%lwt

  Lwt.return_unit

let waq_mstdn_scenario_7_fav =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup me from localhost:3000 *)
  let%lwt aid, _, _ =
    lookup `Mstdn ~token:mstdn_token ~username:"user1" ~domain:waq_server_domain
      ()
  in
  (* Lookup @admin@localhost:3000 *)
  let%lwt admin_id, _username, _acct =
    lookup `Waq ~token:waq_token ~username:"admin" ~domain:"localhost:3000" ()
  in
  (* Follow me from @admin@localhost:3000 *)
  follow `Mstdn ~token:mstdn_token aid;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by me *)
  let%lwt { id = waq_status_id; _ } = post `Waq ~token:waq_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get id of the post *)
  let%lwt mstdn_status_id =
    home_timeline `Mstdn ~token:mstdn_token >|= function
    | [ `Assoc l ] -> List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  (* Favourite the post by @admin@localhost:3000 *)
  let%lwt _ = fav `Mstdn ~token:mstdn_token ~id:mstdn_status_id in
  Lwt_unix.sleep 1.0;%lwt

  (* Check if the post is favourited *)
  (match%lwt get_favourited_by `Waq ~token:waq_token ~id:waq_status_id with
  | [ a ] ->
      assert (a.id = admin_id);
      Lwt.return_unit
  | _ -> assert false);%lwt

  (* Unfavourite the post *)
  let%lwt _ = unfav `Mstdn ~token:mstdn_token ~id:mstdn_status_id in
  Lwt_unix.sleep 1.0;%lwt

  (* Check if the post is unfavourited *)
  (match%lwt get_favourited_by `Waq ~token:waq_token ~id:waq_status_id with
  | [] -> Lwt.return_unit
  | _ -> assert false);%lwt

  Lwt.return_unit

let waq_mstdn_scenario_8_lookup =
  make_waq_and_mstdn_scenario @@ fun waq_token _mstdn_token ->
  (* Lookup @admin@localhost:3000 *)
  let%lwt res1 =
    lookup `Waq ~token:waq_token ~username:"admin" ~domain:"localhost:3000" ()
  in
  let%lwt res2 =
    lookup_via_v1_accounts_search `Waq ~token:waq_token ~username:"admin"
      ~domain:"localhost:3000" ()
  in
  assert (res1 = res2);

  (* Lookup me from localhost:3000 *)
  let%lwt res1 = lookup `Waq ~token:waq_token ~username:"user1" () in
  let%lwt res2 =
    lookup_via_v1_accounts_search `Waq ~token:waq_token ~username:"user1" ()
  in
  assert (res1 = res2);

  Lwt.return_unit

let waq_scenario_1 =
  make_waq_scenario @@ fun _waq_token ->
  let%lwt access_token = fetch_access_token ~username:"user1" in

  let%lwt r =
    fetch_exn
      ~headers:[ (`Authorization, "Bearer " ^ access_token) ]
      (waq "/api/v1/apps/verify_credentials")
  in
  assert (
    match Yojson.Safe.from_string r with
    | `Assoc l -> l |> List.assoc "name" |> expect_string = "foo"
    | _ -> false);

  let%lwt r =
    fetch_exn
      ~headers:[ (`Authorization, "Bearer " ^ access_token) ]
      (waq "/api/v1/accounts/verify_credentials")
  in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  assert (l |> List.assoc "username" |> expect_string = "user1");
  assert (l |> List.assoc "acct" |> expect_string = "user1");
  assert (
    l |> List.assoc "source" |> expect_assoc |> List.assoc "privacy"
    |> expect_string = "public");

  let%lwt r = fetch_exn (waq "/api/v1/instance") in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  assert (l |> List.mem_assoc "uri");

  Lwt.return_unit

let waq_scenario_2 =
  make_waq_scenario @@ fun waq_token ->
  let got_uri = ref None in
  let set_current_state, handler =
    websocket_handler_state_machine ~init:`Init
      ~states:
        [
          (`Init, fun _ -> assert false);
          ( `Recv,
            fun l pushf ->
              assert (List.assoc "stream" l = `List [ `String "user" ]);
              assert (List.assoc "event" l |> expect_string = "update");
              let payload = List.assoc "payload" l |> expect_string in
              let uri =
                let l = Yojson.Safe.from_string payload |> expect_assoc in
                List.assoc "uri" l |> expect_string
              in
              got_uri := Some uri;
              pushf None;%lwt
              Lwt.return `End );
          (`End, fun _ -> assert false);
        ]
      ()
  in

  let expected_uri = ref None in
  let mtx = Lwt_mutex.create () in
  websocket ~mtx `Waq ~token:waq_token handler (fun _pushf ->
      let%lwt { uri; _ } = post `Waq ~token:waq_token () in
      expected_uri := Some uri;
      set_current_state `Recv;
      Lwt.return_unit);%lwt

  assert (Option.get !got_uri = Option.get !expected_uri);
  Lwt.return_unit

let waq_scenario_3 =
  make_waq_scenario @@ fun waq_token ->
  let%lwt waq_token' = fetch_access_token ~username:"user2" in

  (* Look up & Follow @user2 *)
  let%lwt user2_id, _, _ = lookup `Waq ~token:waq_token ~username:"user2" () in
  follow `Waq ~token:waq_token user2_id;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by @user2 *)
  let%lwt { uri; id; _ } = post `Waq ~token:waq_token' () in

  (* Reply by me *)
  let%lwt { uri = uri2; id = id2; _ } =
    post `Waq ~token:waq_token ~in_reply_to_id:id ()
  in

  (* Reply again *)
  let%lwt { uri = uri3; _ } =
    post `Waq ~token:waq_token ~in_reply_to_id:id2 ()
  in

  (* Get my home timeline and check *)
  (home_timeline `Waq ~token:waq_token >|= function
   | [ `Assoc l3; `Assoc l2; `Assoc l ] ->
       (* Check if the timeline is correct *)
       assert (uri = (l |> List.assoc "uri" |> expect_string));
       assert (id = (l2 |> List.assoc "in_reply_to_id" |> expect_string));
       assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
       assert (id2 = (l3 |> List.assoc "in_reply_to_id" |> expect_string));
       assert (uri3 = (l3 |> List.assoc "uri" |> expect_string));
       ()
   | _ -> assert false);%lwt

  (* Unfollow @user2 *)
  unfollow `Waq ~token:waq_token user2_id;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Get my home timeline and check again *)
  (home_timeline `Waq ~token:waq_token >|= function
   | [ `Assoc l3; `Assoc l2 ] ->
       (* Check if the timeline is correct *)
       assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
       assert (uri3 = (l3 |> List.assoc "uri" |> expect_string));
       ()
   | _ -> assert false);%lwt

  (* Check status itself *)
  let%lwt s = get_status `Waq id in
  assert (s.uri = uri);

  (* Check the status's context *)
  let%lwt ancestors, descendants = get_status_context `Waq id2 in
  assert (ancestors |> List.map (fun r -> r.uri) = [ uri ]);
  assert (descendants |> List.map (fun r -> r.uri) = [ uri3 ]);

  Lwt.return_unit

let waq_scenario_4 =
  make_waq_scenario @@ fun token ->
  let ws_recv_ids = ref [] in
  let _, handler =
    websocket_handler_state_machine ~init:`Recv
      ~states:
        [
          ( `Recv,
            fun l _pushf ->
              assert (List.assoc "stream" l = `List [ `String "user" ]);
              assert (List.assoc "event" l |> expect_string = "update");
              let payload = List.assoc "payload" l |> expect_string in
              let s =
                payload |> Yojson.Safe.from_string |> status_of_yojson
                |> Result.get_ok
              in
              ws_recv_ids := s.id :: !ws_recv_ids;
              Lwt.return `Recv );
        ]
      ()
  in

  let expected_ids = ref [] in
  websocket `Waq ~token handler (fun pushf ->
      let%lwt {
            id = id1;
            reblog = None;
            reblogged = false;
            reblogs_count = 0;
            _;
          } =
        post `Waq ~token ~content:"Hello world" ()
      in
      let%lwt {
            id = id2;
            reblogged = true;
            reblog = Some { id = id1'; reblogged = true; reblog = None; _ };
            _;
          } =
        reblog `Waq ~token ~id:id1
      in
      let%lwt { id = id2'; reblog = Some { id = id1''; _ }; _ } =
        reblog `Waq ~token ~id:id1
      in
      let%lwt { id = id2''; reblog = Some { id = id1'''; _ }; _ } =
        reblog `Waq ~token ~id:id2
      in
      assert (id1 = id1' && id1 = id1'' && id1 = id1''');
      assert (id2 = id2' && id2 = id2'');
      expected_ids := [ id1; id2 ];

      pushf None);%lwt

  assert (List.sort compare !expected_ids = List.sort compare !ws_recv_ids);
  Lwt.return_unit
  [@@warning "-8"]

let waq_scenario_5_fav =
  make_waq_scenario @@ fun token ->
  let%lwt { id; account; _ } = post `Waq ~token () in
  let%lwt { favourited; _ } = fav `Waq ~token ~id in
  assert favourited;
  (match%lwt get_favourited_by `Waq ~token ~id with
  | [ a ] ->
      assert (a.id = account.id);
      Lwt.return_unit
  | _ -> assert false);%lwt
  let%lwt { favourited; _ } = unfav `Waq ~token ~id in
  assert (not favourited);
  (match%lwt get_favourited_by `Waq ~token ~id with
  | [] -> Lwt.return_unit
  | _ -> assert false);%lwt
  Lwt.return_unit

let all_tests =
  [
    ("waq-mstdn-1", waq_mstdn_scenario_1);
    ("waq-mstdn-2", waq_mstdn_scenario_2);
    ("waq-mstdn-3", waq_mstdn_scenario_3);
    ("waq-mstdn-4", waq_mstdn_scenario_4);
    ("waq-mstdn-5", waq_mstdn_scenario_5);
    ("waq-mstdn-6", waq_mstdn_scenario_6_fav);
    ("waq-mstdn-7", waq_mstdn_scenario_7_fav);
    ("waq-mstdn-8", waq_mstdn_scenario_8_lookup);
    ("waq-1", waq_scenario_1);
    ("waq-2", waq_scenario_2);
    ("waq-3", waq_scenario_3);
    ("waq-4", waq_scenario_4);
    ("waq-5", waq_scenario_5_fav);
  ]

let execute_one_test (name, f) =
  Logq.debug (fun m -> m "===== Testcase %s =====" name);
  f ()

let () =
  print_newline ();
  Logq.(add_reporter (make_reporter ~l:Debug ()));

  let shuffle d =
    (* Thanks to: https://stackoverflow.com/a/15095713 *)
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond
  in
  let chosen_tests =
    match Sys.argv with
    | [| _ |] -> shuffle all_tests
    | _ ->
        Sys.argv |> Array.to_list |> List.tl
        |> List.map (fun name -> (name, List.assoc name all_tests))
  in

  Logs.info (fun m ->
      chosen_tests |> List.map fst |> String.concat " "
      |> m "[e2e] Chosen tests: %s");

  chosen_tests |> List.iter execute_one_test
