open Common

let f =
  make_waq_scenario @@ fun token ->
  let expected_ids = ref [] in
  let%lwt ws_recv_msgs =
    websocket_stack `Waq ~token @@ fun _pushf ->
    let%lwt { id = id1; reblog = None; reblogged = false; reblogs_count = 0; _ }
        =
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

    let%lwt { reblogs_count; _ } = get_status `Waq ~token id1 in
    assert (reblogs_count = 1);

    Lwt.return_unit
  in

  let ws_recv_msgs =
    ws_recv_msgs |> List.map (Yojson.Safe.from_string |.> expect_assoc)
  in
  let ws_recv_ids, ws_recv_notfs =
    ws_recv_msgs
    |> List.fold_left
         (fun (ws_recv_ids, ws_recv_notfs) l ->
           let payload =
             List.assoc "payload" l |> expect_string |> Yojson.Safe.from_string
           in
           match (List.assoc "stream" l, List.assoc "event" l) with
           | `List [ `String "user" ], `String "update" ->
               let s = payload |> status_of_yojson |> Result.get_ok in
               (s.id :: ws_recv_ids, ws_recv_notfs)
           | `List [ `String "user" ], `String "notification" ->
               let n = payload |> notification_of_yojson |> Result.get_ok in
               (ws_recv_ids, n :: ws_recv_notfs)
           | _ -> (ws_recv_ids, ws_recv_notfs))
         ([], [])
  in

  assert (List.sort compare !expected_ids = List.sort compare ws_recv_ids);
  assert (ws_recv_notfs = []);

  Lwt.return_unit
  [@@warning "-8"]
