open Common

let f =
  make_waq_scenario @@ fun env token ->
  let expected_ids = ref [] in
  let ws_recv_msgs =
    websocket_stack env `Waq ~token @@ fun _pushf ->
    let { id = id1; reblog = None; reblogged = false; reblogs_count = 0; _ } =
      post env `Waq ~token ~content:"Hello world" ()
    in
    let {
      id = id2;
      reblogged = true;
      reblog = Some { id = id1'; reblogged = true; reblog = None; _ };
      _;
    } =
      reblog env `Waq ~token ~id:id1
    in
    let { id = id2'; reblog = Some { id = id1''; _ }; _ } =
      reblog env `Waq ~token ~id:id1
    in
    let { id = id2''; reblog = Some { id = id1'''; _ }; _ } =
      reblog env `Waq ~token ~id:id2
    in
    assert (id1 = id1' && id1 = id1'' && id1 = id1''');
    assert (id2 = id2' && id2 = id2'');
    expected_ids := [ id1; id2 ];

    let { reblogs_count; _ } = get_status env `Waq ~token id1 in
    assert (reblogs_count = 1);

    ()
      [@@warning "-8"]
  in

  let ws_recv_msgs =
    ws_recv_msgs
    |> List.map (fun x -> x |> Yojson.Safe.from_string |> expect_assoc)
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
               let s = payload |> status_of_yojson in
               (s.id :: ws_recv_ids, ws_recv_notfs)
           | `List [ `String "user" ], `String "notification" ->
               let n = payload |> notification_of_yojson in
               (ws_recv_ids, n :: ws_recv_notfs)
           | _ -> (ws_recv_ids, ws_recv_notfs))
         ([], [])
  in

  assert (List.sort compare !expected_ids = List.sort compare ws_recv_ids);
  assert (ws_recv_notfs = []);

  ()
