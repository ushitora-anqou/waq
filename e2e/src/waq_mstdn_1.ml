open Common

let f =
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
      (* Lookup @mstdn1@mstdn_server_domain *)
      let%lwt mstdn1_id, username, acct =
        lookup `Waq ~token:waq_token ~username:"mstdn1"
          ~domain:mstdn_server_domain ()
      in
      assert (username = "mstdn1");
      assert (acct = "mstdn1@" ^ mstdn_server_domain);

      (* Follow @mstdn1@mstdn_server_domain *)
      follow `Waq ~token:waq_token mstdn1_id;%lwt
      Lwt_unix.sleep 1.0;%lwt

      (* Post by @mstdn1@mstdn_server_domain *)
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
       | res ->
           Logq.err (fun m ->
               m "unexpected home timeline %s"
                 (Yojson.Safe.to_string (`List res)));
           assert false);%lwt

      (* Unfollow @mstdn1@mstdn_server_domain *)
      unfollow `Waq ~token:waq_token mstdn1_id;%lwt
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
