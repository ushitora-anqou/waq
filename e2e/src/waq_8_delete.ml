open Common

let expect_no_status env kind id =
  try
    get_status env kind id |> ignore;
    assert false
  with FetchFailure (Some (`Not_found, _, _)) -> ()

let f =
  make_waq_scenario @@ fun env token ->
  let token' = fetch_access_token env ~username:"user2" in
  let ws_recv_msgs =
    websocket_stack env `Waq ~token @@ fun _pushf ->
    let ({ id; _ } : status) = post env `Waq ~token () in
    let s = get_status env `Waq id in
    assert (s.id = id);

    (* Wrong delete *)
    expect_exc (fun () -> delete_status env `Waq ~token:token' id);
    (* Should remain *)
    get_status env `Waq id |> ignore;

    (* Actual delete *)
    let s = delete_status env `Waq ~token id in
    assert (s.id = id);
    expect_no_status env `Waq id;

    let ({ id; _ } : status) = post env `Waq ~token () in
    let s = reblog env `Waq ~token ~id in
    delete_status env `Waq ~token id |> ignore;
    expect_no_status env `Waq id;
    expect_no_status env `Waq s.id;

    ()
  in
  let ws_delete_events =
    ws_recv_msgs
    |> List.map (fun x -> x |> Yojson.Safe.from_string |> expect_assoc)
    |> List.filter_map (fun (l : (string * Yojson.Safe.t) list) ->
           if
             List.assoc "stream" l = `List [ `String "user" ]
             && List.assoc "event" l = `String "delete"
           then Some (List.assoc "payload" l |> expect_string)
           else None)
  in
  assert (
    List.sort compare ws_delete_events = List.sort compare [ "1"; "2"; "3" ]);

  ()
