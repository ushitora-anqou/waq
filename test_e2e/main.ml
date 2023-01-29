module Log = Waq.Log
module Uri = Waq.Http.Uri
module Ptime = Waq.Util.Ptime

let ignore_lwt = Waq.Util.ignore_lwt
let fetch = Waq.Http.fetch
let fetch_exn = Waq.Http.fetch_exn

let expect_string = function
  | `String s -> s
  | _ -> failwith "Expected string, got something different"

let expect_assoc = function
  | `Assoc l -> l
  | _ -> failwith "Expected assoc, got something different"

let with_lock mtx f =
  match mtx with None -> f () | Some mtx -> Lwt_mutex.with_lock mtx f

let websocket ?mtx uri handler f =
  let open Websocket_lwt_unix in
  let uri = Uri.of_string uri in
  let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let%lwt client = Conduit_lwt_unix.endp_to_client ~ctx endp in
  let%lwt conn = connect ~ctx client uri in
  let close_sent = ref false in
  let pushf msg =
    match msg with
    | Some content -> write conn (Websocket.Frame.create ~content ())
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
      Log.debug (fun m -> m "Killing mastodon processes");
      kill pid Sys.sigint;
      close_process_in ic |> ignore)

let waq_server_name = Sys.getenv "WAQ_SERVER_NAME"
let waq_server_domain = Uri.(of_string waq_server_name |> domain)
let waq url = waq_server_name ^ url

let mstdn url =
  let server_name = "http://localhost:3000" in
  server_name ^ url

let pp_json (s : string) =
  Log.debug (fun m -> m "%s" Yojson.Safe.(from_string s |> pretty_to_string))
  [@@warning "-32"]

let waq_mstdn_scenario_1 waq_token mstdn_token =
  let waq_auth = ("Authorization", "Bearer " ^ waq_token) in
  let mstdn_auth = ("Authorization", "Bearer " ^ mstdn_token) in

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
  let target =
    Printf.sprintf "/api/v1/streaming?access_token=%s&stream=user" waq_token
  in
  let uris = ref [] in
  websocket (waq target) handler (fun pushf ->
      (* Lookup @admin@localhost:3000 *)
      let%lwt r =
        fetch_exn
          (waq "/api/v1/accounts/search?q=@admin@localhost:3000&resolve=true")
      in
      let admin_id, username, acct =
        let l =
          match Yojson.Safe.from_string r with
          | `List [ `Assoc l ] -> l
          | _ -> assert false
        in
        ( l |> List.assoc "id" |> expect_string,
          l |> List.assoc "username" |> expect_string,
          l |> List.assoc "acct" |> expect_string )
      in
      assert (username = "admin");
      assert (acct = "admin@localhost:3000");

      (* Follow @admin@localhost:3000 *)
      fetch_exn ~meth:`POST ~headers:[ waq_auth ]
        (waq (Printf.sprintf "/api/v1/accounts/%s/follow" admin_id))
      |> ignore_lwt;%lwt
      Lwt_unix.sleep 1.0;%lwt

      (* Post by @admin@localhost:3000 *)
      let content = "こんにちは、世界！" in
      let%lwt r =
        let body =
          `Assoc [ ("status", `String content) ] |> Yojson.Safe.to_string
        in
        fetch_exn
          ~headers:
            [
              mstdn_auth;
              ("Accept", "application/json");
              ("Content-Type", "application/json");
            ]
          ~meth:`POST ~body (mstdn "/api/v1/statuses")
      in
      let uri, content =
        let l = Yojson.Safe.from_string r |> expect_assoc in
        ( List.assoc "uri" l |> expect_string,
          List.assoc "content" l |> expect_string )
      in
      uris := uri :: !uris;
      Lwt_unix.sleep 1.0;%lwt

      (* Post by me *)
      let content2 = "こんにちは、世界！２" in
      let%lwt r =
        let body =
          `Assoc [ ("status", `String content2) ] |> Yojson.Safe.to_string
        in
        fetch_exn
          ~headers:
            [
              waq_auth;
              ("Accept", "application/json");
              ("Content-Type", "application/json");
            ]
          ~meth:`POST ~body (waq "/api/v1/statuses")
      in
      let uri2, content2 =
        let l = Yojson.Safe.from_string r |> expect_assoc in
        ( List.assoc "uri" l |> expect_string,
          List.assoc "content" l |> expect_string )
      in
      uris := uri2 :: !uris;
      Lwt_unix.sleep 1.0;%lwt

      (* Get my home timeline and check *)
      let%lwt r =
        fetch_exn ~headers:[ waq_auth ] (waq "/api/v1/timelines/home")
      in
      (match Yojson.Safe.from_string r with
      | `List [ `Assoc l2; `Assoc l ] ->
          (* Check if the timeline is correct *)
          assert (uri = (l |> List.assoc "uri" |> expect_string));
          assert (content = (l |> List.assoc "content" |> expect_string));
          assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
          assert (content2 = (l2 |> List.assoc "content" |> expect_string));
          ()
      | _ -> assert false);

      (* Unfollow @admin@localhost:3000 *)
      fetch_exn ~meth:`POST ~headers:[ waq_auth ]
        (waq (Printf.sprintf "/api/v1/accounts/%s/unfollow" admin_id))
      |> ignore_lwt;%lwt
      Lwt_unix.sleep 1.0;%lwt

      (* Get my home timeline and check again *)
      let%lwt r =
        fetch_exn ~headers:[ waq_auth ] (waq "/api/v1/timelines/home")
      in
      (match Yojson.Safe.from_string r with
      | `List [ `Assoc l2 ] ->
          (* Check if the timeline is correct *)
          assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
          assert (content2 = (l2 |> List.assoc "content" |> expect_string));
          ()
      | _ -> assert false);

      pushf None);%lwt

  let expected_uris = List.sort compare !uris in
  let got_uris =
    !ws_statuses
    |> List.map (fun s -> s |> List.assoc "uri" |> expect_string)
    |> List.sort compare
  in
  assert (expected_uris = got_uris);

  Lwt.return_unit

let waq_mstdn_scenario_2 waq_token mstdn_token =
  let waq_auth = ("Authorization", "Bearer " ^ waq_token) in
  let mstdn_auth = ("Authorization", "Bearer " ^ mstdn_token) in

  (* Lookup me from localhost:3000 *)
  let%lwt r =
    fetch_exn ~headers:[ mstdn_auth ]
      (mstdn "/api/v1/accounts/search?q=@user1@"
      ^ waq_server_domain ^ "&resolve=true")
  in
  let aid =
    match Yojson.Safe.from_string r with
    | `List [ `Assoc l ] -> l |> List.assoc "id" |> expect_string
    | _ ->
        pp_json r;
        assert false
  in

  (* Follow me from @admin@localhost:3000 *)
  let%lwt _ =
    fetch_exn ~meth:`POST ~headers:[ mstdn_auth ]
      (mstdn ("/api/v1/accounts/" ^ aid ^ "/follow"))
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Post by @admin@localhost:3000 *)
  let content = "こんにちは、世界！" in
  let%lwt r =
    let body =
      `Assoc [ ("status", `String content) ] |> Yojson.Safe.to_string
    in
    fetch_exn
      ~headers:
        [
          mstdn_auth;
          ("Accept", "application/json");
          ("Content-Type", "application/json");
        ]
      ~meth:`POST ~body (mstdn "/api/v1/statuses")
  in
  let uri, content =
    match Yojson.Safe.from_string r with
    | `Assoc l -> (List.assoc "uri" l, List.assoc "content" l)
    | _ -> assert false
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Post by me *)
  let content2 = "こんにちは、世界！２" in
  let%lwt r =
    let body =
      `Assoc [ ("status", `String content2) ] |> Yojson.Safe.to_string
    in
    fetch_exn
      ~headers:
        [
          waq_auth;
          ("Accept", "application/json");
          ("Content-Type", "application/json");
        ]
      ~meth:`POST ~body (waq "/api/v1/statuses")
  in
  let uri2, content2 =
    match Yojson.Safe.from_string r with
    | `Assoc l -> (List.assoc "uri" l, List.assoc "content" l)
    | _ -> assert false
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 and check *)
  let%lwt r =
    fetch_exn ~headers:[ mstdn_auth ] (mstdn "/api/v1/timelines/home")
  in
  (match Yojson.Safe.from_string r with
  | `List [ `Assoc l2; `Assoc l ] ->
      (* Check if the timeline is correct *)
      assert (uri = List.assoc "uri" l);
      assert (content = List.assoc "content" l);
      assert (uri2 = List.assoc "uri" l2);
      assert (content2 = List.assoc "content" l2);
      ()
  | _ -> assert false);

  (* Unfollow me from @admin@localhost:3000 *)
  let%lwt _ =
    fetch_exn ~meth:`POST ~headers:[ mstdn_auth ]
      (mstdn ("/api/v1/accounts/" ^ aid ^ "/unfollow"))
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 and check again *)
  let%lwt r =
    fetch_exn ~headers:[ mstdn_auth ] (mstdn "/api/v1/timelines/home")
  in
  (match Yojson.Safe.from_string r with
  | `List [ `Assoc l ] ->
      (* Check if the timeline is correct *)
      assert (uri = List.assoc "uri" l);
      assert (content = List.assoc "content" l);
      ()
  | _ -> assert false);

  Lwt.return_unit

let fetch_access_token ~username =
  let%lwt r =
    fetch_exn ~meth:`POST
      ~headers:[ ("Content-Type", "application/json") ]
      ~body:{|{"client_name":"foo","redirect_uris":"http://example.com"}|}
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
          ("redirect_uri", [ "http://example.com" ]);
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
        let loc = headers |> List.assoc "location" in
        String.sub loc 24 (String.length loc - 24)
    | _ -> assert false
  in

  let%lwt r =
    fetch_exn ~meth:`POST
      ~headers:[ ("Content-Type", "application/json") ]
      ~body:
        (`Assoc
           [
             ("grant_type", `String "authorization_code");
             ("code", `String auth_code);
             ("client_id", `String client_id);
             ("client_secret", `String client_secret);
             ("redirect_uri", `String "http://example.com");
           ]
        |> Yojson.Safe.to_string)
      (waq "/oauth/token")
  in
  match Yojson.Safe.from_string r with
  | `Assoc l -> l |> List.assoc "access_token" |> expect_string |> Lwt.return
  | _ -> assert false

let waq_scenario_1 _waq_token =
  let%lwt access_token = fetch_access_token ~username:"user1" in
  let%lwt r =
    fetch_exn
      ~headers:[ ("Authorization", "Bearer " ^ access_token) ]
      (waq "/api/v1/apps/verify_credentials")
  in
  assert (
    match Yojson.Safe.from_string r with
    | `Assoc l -> l |> List.assoc "name" |> expect_string = "foo"
    | _ -> false);

  Lwt.return_unit

let waq_scenario_2 waq_token =
  let waq_auth = ("Authorization", "Bearer " ^ waq_token) in
  let target =
    Printf.sprintf "/api/v1/streaming?access_token=%s&stream=user" waq_token
  in

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
  websocket ~mtx (waq target) handler (fun _pushf ->
      let content = "こんにちは、世界！" in
      let%lwt r =
        let body =
          `Assoc [ ("status", `String content) ] |> Yojson.Safe.to_string
        in
        fetch_exn
          ~headers:
            [
              waq_auth;
              ("Accept", "application/json");
              ("Content-Type", "application/json");
            ]
          ~meth:`POST ~body (waq "/api/v1/statuses")
      in
      expected_uri :=
        Yojson.Safe.from_string r |> expect_assoc |> List.assoc "uri"
        |> expect_string |> Option.some;
      set_current_state `Recv;
      Lwt.return_unit);%lwt

  assert (Option.get !got_uri = Option.get !expected_uri);
  Lwt.return_unit

let waq_scenario_3 waq_token =
  let waq_auth = ("Authorization", "Bearer " ^ waq_token) in
  let%lwt waq_token' = fetch_access_token ~username:"user2" in
  let waq_auth' = ("Authorization", "Bearer " ^ waq_token') in

  (* Look up & Follow @user2 *)
  let%lwt r = fetch_exn (waq "/api/v1/accounts/search?q=@user2") in
  let user2_id =
    match Yojson.Safe.from_string r with
    | `List [ `Assoc l ] -> l |> List.assoc "id" |> expect_string
    | _ -> assert false
  in
  fetch_exn ~meth:`POST ~headers:[ waq_auth ]
    (waq (Printf.sprintf "/api/v1/accounts/%s/follow" user2_id))
  |> ignore_lwt;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by @user2 *)
  let content = "こんにちは、世界！" in
  let%lwt r =
    let body =
      `Assoc [ ("status", `String content) ] |> Yojson.Safe.to_string
    in
    fetch_exn
      ~headers:
        [
          waq_auth';
          ("Accept", "application/json");
          ("Content-Type", "application/json");
        ]
      ~meth:`POST ~body (waq "/api/v1/statuses")
  in
  let uri =
    let l = Yojson.Safe.from_string r |> expect_assoc in
    List.assoc "uri" l |> expect_string
  in

  (* Post by me *)
  let content2 = "こんにちは、世界！２" in
  let%lwt r =
    let body =
      `Assoc [ ("status", `String content2) ] |> Yojson.Safe.to_string
    in
    fetch_exn
      ~headers:
        [
          waq_auth;
          ("Accept", "application/json");
          ("Content-Type", "application/json");
        ]
      ~meth:`POST ~body (waq "/api/v1/statuses")
  in
  let uri2 =
    let l = Yojson.Safe.from_string r |> expect_assoc in
    List.assoc "uri" l |> expect_string
  in

  (* Get my home timeline and check *)
  let%lwt r = fetch_exn ~headers:[ waq_auth ] (waq "/api/v1/timelines/home") in
  (match Yojson.Safe.from_string r with
  | `List [ `Assoc l2; `Assoc l ] ->
      (* Check if the timeline is correct *)
      assert (uri = (l |> List.assoc "uri" |> expect_string));
      assert (content = (l |> List.assoc "content" |> expect_string));
      assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
      assert (content2 = (l2 |> List.assoc "content" |> expect_string));
      ()
  | _ -> assert false);

  (* Unfollow @user2 *)
  fetch_exn ~meth:`POST ~headers:[ waq_auth ]
    (waq (Printf.sprintf "/api/v1/accounts/%s/unfollow" user2_id))
  |> ignore_lwt;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Get my home timeline and check again *)
  let%lwt r = fetch_exn ~headers:[ waq_auth ] (waq "/api/v1/timelines/home") in
  (match Yojson.Safe.from_string r with
  | `List [ `Assoc l2 ] ->
      (* Check if the timeline is correct *)
      assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
      assert (content2 = (l2 |> List.assoc "content" |> expect_string));
      ()
  | _ -> assert false);

  Lwt.return_unit

let scenarios_with_waq_and_mstdn () =
  [ (1, waq_mstdn_scenario_1); (2, waq_mstdn_scenario_2) ]
  |> List.iter @@ fun (i, scenario) ->
     Log.debug (fun m -> m "===== Scenario waq-mstdn-%d =====" i);
     new_session @@ fun waq_token ->
     Log.debug (fun m -> m "Access token for Waq: %s" waq_token);
     new_mastodon_session @@ fun mstdn_token ->
     Log.debug (fun m -> m "Access token for Mastodon: %s" mstdn_token);
     Unix.sleep 10;
     Lwt_main.run @@ scenario waq_token mstdn_token

let scenarios_with_waq () =
  [ (1, waq_scenario_1); (2, waq_scenario_2); (3, waq_scenario_3) ]
  |> List.iter @@ fun (i, scenario) ->
     Log.debug (fun m -> m "===== Scenario waq-%d =====" i);
     new_session @@ fun waq_token ->
     Log.debug (fun m -> m "Access token for Waq: %s" waq_token);
     Unix.sleep 1;
     Lwt_main.run @@ scenario waq_token

let () =
  print_newline ();
  Log.(add_reporter (make_reporter ~l:Debug ()));
  scenarios_with_waq ();
  scenarios_with_waq_and_mstdn ();
  ()
