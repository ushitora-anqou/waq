open Common

let expect_followers account_id expected_follower_ids =
  let%lwt l = get_followers `Waq account_id in
  let got = l |> List.map (fun (a : account) -> a.id) in
  assert (got = expected_follower_ids);
  Lwt.return_unit

let expect_following account_id expected_follower_ids =
  let%lwt l = get_following `Waq account_id in
  let got = l |> List.map (fun (a : account) -> a.id) in
  assert (got = expected_follower_ids);
  Lwt.return_unit

let f =
  make_waq_scenario @@ fun token ->
  (* Connect WebSocket *)
  let%lwt ws_recv_msgs =
    websocket_stack `Waq ~token @@ fun _pushf ->
    let%lwt token' = fetch_access_token ~username:"user2" in
    let%lwt user1_id, _, _ = lookup `Waq ~token:token' ~username:"user1" () in
    let%lwt user2_id, _, _ = lookup `Waq ~token ~username:"user2" () in

    (* user1: Try to follow myself, which should be forbidden *)
    (try%lwt
       follow `Waq ~token user1_id;%lwt
       assert false
     with Httpq.Client.FetchFailure (Some (`Forbidden, _, _)) ->
       Lwt.return_unit);%lwt

    (* user1: Follow @user2 *)
    follow `Waq ~token user2_id;%lwt
    expect_followers user2_id [ user1_id ];%lwt
    expect_following user1_id [ user2_id ];%lwt

    (* user1: check relationship *)
    (match%lwt get_relationships `Waq ~token [ user2_id ] with
    | [ rel ] ->
        assert (rel.id = user2_id);
        assert rel.following;
        assert (not rel.followed_by);
        Lwt.return_unit
    | _ -> assert false);%lwt

    (* check accounts *)
    let%lwt a = get_account `Waq user1_id in
    assert (a.followers_count = 0);
    assert (a.following_count = 1);
    let%lwt a = get_account `Waq user2_id in
    assert (a.followers_count = 1);
    assert (a.following_count = 0);

    (* check notifications *)
    (match%lwt get_notifications `Waq ~token:token' with
    | [ { typ = "follow"; account = a; _ } ] ->
        assert (a.id = user1_id);
        Lwt.return_unit
    | _ -> assert false);%lwt

    (* user2: follow @user1 *)
    follow `Waq ~token:token' user1_id;%lwt
    expect_followers user1_id [ user2_id ];%lwt
    expect_following user2_id [ user1_id ];%lwt

    (* user1: check relationship *)
    (match%lwt get_relationships `Waq ~token [ user2_id ] with
    | [ rel ] ->
        assert (rel.id = user2_id);
        assert rel.following;
        assert rel.followed_by;
        Lwt.return_unit
    | _ -> assert false);%lwt

    (* check accounts *)
    let%lwt a = get_account `Waq user1_id in
    assert (a.followers_count = 1);
    assert (a.following_count = 1);
    let%lwt a = get_account `Waq user2_id in
    assert (a.followers_count = 1);
    assert (a.following_count = 1);

    (* check notifications *)
    (match%lwt get_notifications `Waq ~token with
    | [ { typ = "follow"; account = a; _ } ] ->
        assert (a.id = user2_id);
        Lwt.return_unit
    | _ -> assert false);%lwt

    (* user1: Unfollow @user2 *)
    unfollow `Waq ~token user2_id;%lwt
    expect_followers user2_id [];%lwt
    expect_following user1_id [];%lwt

    (* user1: check relationship *)
    (match%lwt get_relationships `Waq ~token [ user2_id ] with
    | [ rel ] ->
        assert (rel.id = user2_id);
        assert (not rel.following);
        assert rel.followed_by;
        Lwt.return_unit
    | _ -> assert false);%lwt

    (* check accounts *)
    let%lwt a = get_account `Waq user1_id in
    assert (a.followers_count = 1);
    assert (a.following_count = 0);
    let%lwt a = get_account `Waq user2_id in
    assert (a.followers_count = 0);
    assert (a.following_count = 1);

    (* user2: Unfollow @user1 *)
    unfollow `Waq ~token:token' user1_id;%lwt
    expect_followers user1_id [];%lwt
    expect_following user2_id [];%lwt

    (* user1: check relationship *)
    (match%lwt get_relationships `Waq ~token [ user2_id ] with
    | [ rel ] ->
        assert (rel.id = user2_id);
        assert (not rel.following);
        assert (not rel.followed_by);
        Lwt.return_unit
    | _ -> assert false);%lwt

    (* check accounts *)
    let%lwt a = get_account `Waq user1_id in
    assert (a.followers_count = 0);
    assert (a.following_count = 0);
    let%lwt a = get_account `Waq user2_id in
    assert (a.followers_count = 0);
    assert (a.following_count = 0);

    Lwt.return_unit
  in

  let ws_notifications =
    ws_recv_msgs
    |> List.map (Yojson.Safe.from_string |.> expect_assoc)
    |> List.filter_map (fun (l : (string * Yojson.Safe.t) list) ->
           if
             List.assoc "stream" l = `List [ `String "user" ]
             && List.assoc "event" l = `String "notification"
           then
             Some
               (List.assoc "payload" l |> expect_string
              |> Yojson.Safe.from_string |> notification_of_yojson
              |> Result.get_ok)
           else None)
  in
  let%lwt got_notifications = get_notifications `Waq ~token in
  assert (
    got_notifications
    |> List.map (fun r -> r.id)
    |> List.sort compare
    = (ws_notifications |> List.map (fun r -> r.id) |> List.sort compare));

  Lwt.return_unit
