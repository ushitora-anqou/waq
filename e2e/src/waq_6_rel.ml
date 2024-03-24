open Common

let expect_followers env account_id expected_follower_ids =
  let l = get_followers env `Waq account_id in
  let got = l |> List.map (fun (a : account) -> a.id) in
  assert (got = expected_follower_ids);
  ()

let expect_following env account_id expected_follower_ids =
  let l = get_following env `Waq account_id in
  let got = l |> List.map (fun (a : account) -> a.id) in
  assert (got = expected_follower_ids);
  ()

let f =
  make_waq_scenario @@ fun env token ->
  (* Connect WebSocket *)
  let ws_recv_msgs =
    websocket_stack env `Waq ~token @@ fun _pushf ->
    let token' = fetch_access_token env ~username:"user2" in
    let user1_id, _, _ = lookup env `Waq ~token:token' ~username:"user1" () in
    let user2_id, _, _ = lookup env `Waq ~token ~username:"user2" () in
    let user3_id, _, _ = lookup env `Waq ~token ~username:"user3" () in

    (* user1: Try to follow myself, which should be forbidden *)
    (try
       follow env `Waq ~token user1_id;
       assert false
     with FetchFailure (Some (`Forbidden, _, _)) -> ());

    (* user1: Follow @user2 *)
    follow env `Waq ~token user2_id;
    expect_followers env user2_id [ user1_id ];
    expect_following env user1_id [ user2_id ];

    (* user1: check relationship *)
    (match get_relationships env `Waq ~token [ user2_id; user3_id ] with
    | [ rel2; rel3 ] ->
        assert (rel2.id = user2_id);
        assert (rel3.id = user3_id);
        assert rel2.following;
        assert (not rel2.followed_by);
        ()
    | _ -> assert false);

    (* check accounts *)
    let a = get_account env `Waq user1_id in
    assert (a.followers_count = 0);
    assert (a.following_count = 1);
    let a = get_account env `Waq user2_id in
    assert (a.followers_count = 1);
    assert (a.following_count = 0);

    (* check notifications *)
    (match get_notifications env `Waq ~token:token' with
    | [ { typ = "follow"; account = a; _ } ] ->
        assert (a.id = user1_id);
        ()
    | _ -> assert false);

    (* user2: follow @user1 *)
    follow env `Waq ~token:token' user1_id;
    expect_followers env user1_id [ user2_id ];
    expect_following env user2_id [ user1_id ];

    (* user1: check relationship *)
    (match get_relationships env `Waq ~token [ user2_id ] with
    | [ rel ] ->
        assert (rel.id = user2_id);
        assert rel.following;
        assert rel.followed_by;
        ()
    | _ -> assert false);

    (* check accounts *)
    let a = get_account env `Waq user1_id in
    assert (a.followers_count = 1);
    assert (a.following_count = 1);
    let a = get_account env `Waq user2_id in
    assert (a.followers_count = 1);
    assert (a.following_count = 1);

    (* check notifications *)
    (match get_notifications env `Waq ~token with
    | [ { typ = "follow"; account = a; _ } ] ->
        assert (a.id = user2_id);
        ()
    | _ -> assert false);

    (* user1: Unfollow @user2 *)
    unfollow env `Waq ~token user2_id;
    expect_followers env user2_id [];
    expect_following env user1_id [];

    (* user1: check relationship *)
    (match get_relationships env `Waq ~token [ user2_id ] with
    | [ rel ] ->
        assert (rel.id = user2_id);
        assert (not rel.following);
        assert rel.followed_by;
        ()
    | _ -> assert false);

    (* check accounts *)
    let a = get_account env `Waq user1_id in
    assert (a.followers_count = 1);
    assert (a.following_count = 0);
    let a = get_account env `Waq user2_id in
    assert (a.followers_count = 0);
    assert (a.following_count = 1);

    (* user2: Unfollow @user1 *)
    unfollow env `Waq ~token:token' user1_id;
    expect_followers env user1_id [];
    expect_following env user2_id [];

    (* user1: check relationship *)
    (match get_relationships env `Waq ~token [ user2_id ] with
    | [ rel ] ->
        assert (rel.id = user2_id);
        assert (not rel.following);
        assert (not rel.followed_by);
        ()
    | _ -> assert false);

    (* check accounts *)
    let a = get_account env `Waq user1_id in
    assert (a.followers_count = 0);
    assert (a.following_count = 0);
    let a = get_account env `Waq user2_id in
    assert (a.followers_count = 0);
    assert (a.following_count = 0);

    ()
  in

  let ws_notifications =
    ws_recv_msgs
    |> List.map (fun x -> x |> Yojson.Safe.from_string |> expect_assoc)
    |> List.filter_map (fun (l : (string * Yojson.Safe.t) list) ->
           if
             List.assoc "stream" l = `List [ `String "user" ]
             && List.assoc "event" l = `String "notification"
           then
             Some
               (List.assoc "payload" l |> expect_string
              |> Yojson.Safe.from_string |> notification_of_yojson)
           else None)
  in
  let got_notifications = get_notifications env `Waq ~token in
  assert (
    got_notifications
    |> List.map (fun r -> r.id)
    |> List.sort compare
    = (ws_notifications |> List.map (fun r -> r.id) |> List.sort compare));

  ()
