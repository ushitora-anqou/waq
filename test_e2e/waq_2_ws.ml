open Common

let f =
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
