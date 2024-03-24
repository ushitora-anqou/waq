open Common

open struct
  let f ~use_query_param =
    make_waq_scenario @@ fun env waq_token ->
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

                (* Check that no event should be received once an unsubscribe message is sent. *)
                if not use_query_param then (
                  pushf (Some {|{"type":"unsubscribe","stream":"user"}|});
                  let _ = post env `Waq ~token:waq_token () in
                  Eio.Time.sleep (Eio.Stdenv.clock env) 5.0);

                pushf None;
                `End );
            (`End, fun _ -> assert false);
          ]
        ()
    in

    let target =
      if use_query_param then "/api/v1/streaming?stream=user"
      else "/api/v1/streaming"
    in

    let expected_uri = ref None in
    let mtx = Eio.Mutex.create () in
    websocket env ~mtx `Waq ~target ~token:waq_token handler (fun pushf ->
        if not use_query_param then
          pushf (Some {|{"type":"subscribe","stream":"user"}|});
        let { uri; _ } = post env `Waq ~token:waq_token () in
        expected_uri := Some uri;
        set_current_state `Recv);

    assert (Option.get !got_uri = Option.get !expected_uri);
    ()
end

let f1 = f ~use_query_param:true
let f2 = f ~use_query_param:false
