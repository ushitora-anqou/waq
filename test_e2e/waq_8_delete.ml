open Common

let expect_no_status kind id =
  try%lwt
    get_status kind id |> ignore_lwt;%lwt
    assert false
  with Httpq.Client.FetchFailure (Some (`Not_found, _, _)) -> Lwt.return_unit

let f =
  make_waq_scenario @@ fun token ->
  let%lwt token' = fetch_access_token ~username:"user2" in
  let%lwt ws_recv_msgs =
    websocket_stack `Waq ~token @@ fun _pushf ->
    let%lwt { id; _ } = post `Waq ~token () in
    let%lwt s = get_status `Waq id in
    assert (s.id = id);

    (* Wrong delete *)
    (try%lwt
       delete_status `Waq ~token:token' id |> ignore_lwt;%lwt
       assert false
     with _ -> Lwt.return_unit);%lwt
    (* Should remain *)
    get_status `Waq id |> ignore_lwt;%lwt

    (* Actual delete *)
    let%lwt s = delete_status `Waq ~token id in
    assert (s.id = id);
    expect_no_status `Waq id;%lwt

    let%lwt { id; _ } = post `Waq ~token () in
    let%lwt s = reblog `Waq ~token ~id in
    delete_status `Waq ~token id |> ignore_lwt;%lwt
    expect_no_status `Waq id;%lwt
    expect_no_status `Waq s.id;%lwt

    Lwt.return_unit
  in
  let ws_delete_events =
    ws_recv_msgs
    |> List.map (Yojson.Safe.from_string |.> expect_assoc)
    |> List.filter_map (fun (l : (string * Yojson.Safe.t) list) ->
           if
             List.assoc "stream" l = `List [ `String "user" ]
             && List.assoc "event" l = `String "delete"
           then Some (List.assoc "payload" l |> expect_string)
           else None)
  in
  assert (
    List.sort compare ws_delete_events = List.sort compare [ "1"; "2"; "3" ]);

  Lwt.return_unit
