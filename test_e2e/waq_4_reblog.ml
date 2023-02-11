open Common

let f =
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
