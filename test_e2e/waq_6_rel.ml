open Common

let f =
  make_waq_scenario @@ fun token ->
  let%lwt token' = fetch_access_token ~username:"user2" in
  let%lwt user1_id, _, _ = lookup `Waq ~token:token' ~username:"user1" () in
  let%lwt user2_id, _, _ = lookup `Waq ~token ~username:"user2" () in

  (* user1: Follow @user2 *)
  follow `Waq ~token user2_id;%lwt

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

  (* user2: follow @user1 *)
  follow `Waq ~token:token' user1_id;%lwt

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

  (* user1: Unfollow @user2 *)
  unfollow `Waq ~token user2_id;%lwt

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
