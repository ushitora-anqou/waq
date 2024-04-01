open Common

let f =
  make_waq_and_mstdn_scenario @@ fun env waq_token mstdn_token ->
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
              `Recv );
        ]
      ()
  in
  let uris = ref [] in
  websocket env `Waq ~token:waq_token handler (fun pushf ->
      (* Lookup @mstdn1@mstdn_server_domain *)
      let mstdn1_id, username, acct =
        lookup env `Waq ~token:waq_token ~username:"mstdn1"
          ~domain:mstdn_server_domain ()
      in
      assert (username = "mstdn1");
      assert (acct = "mstdn1@" ^ mstdn_server_domain);

      (* Follow @mstdn1@mstdn_server_domain *)
      follow env `Waq ~token:waq_token mstdn1_id;
      Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

      (* Post by @mstdn1@mstdn_server_domain *)
      let ({ uri; _ } : status) = post env `Mstdn ~token:mstdn_token () in
      uris := uri :: !uris;
      Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

      (* Post by me *)
      let ({ uri = uri2; _ } : status) = post env `Waq ~token:waq_token () in
      uris := uri2 :: !uris;
      Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

      (* Get my home timeline and check *)
      (home_timeline env `Waq ~token:waq_token |> function
       | [ `Assoc l2; `Assoc l ] ->
           (* Check if the timeline is correct *)
           assert (uri = (l |> List.assoc "uri" |> expect_string));
           assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
           ()
       | res ->
           Logs.err (fun m ->
               m "unexpected home timeline %s"
                 (Yojson.Safe.to_string (`List res)));
           assert false);

      (* Unfollow @mstdn1@mstdn_server_domain *)
      unfollow env `Waq ~token:waq_token mstdn1_id;
      Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

      (* Get my home timeline and check again *)
      (home_timeline env `Waq ~token:waq_token |> function
       | [ `Assoc l2 ] ->
           (* Check if the timeline is correct *)
           assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
           ()
       | _ -> assert false);

      pushf None);

  let expected_uris = List.sort compare !uris in
  let got_uris =
    !ws_statuses
    |> List.map (fun s -> s |> List.assoc "uri" |> expect_string)
    |> List.sort compare
  in
  assert (expected_uris = got_uris);

  ()
