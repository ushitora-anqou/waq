open Common

let f =
  make_waq_scenario @@ fun token ->
  let%lwt token' = fetch_access_token ~username:"user2" in

  (* user1: Look up & Follow @user2 *)
  let%lwt user2_id, _, _ = lookup `Waq ~token ~username:"user2" () in
  follow `Waq ~token user2_id;%lwt

  (* user1: check relationship *)
  (match%lwt get_relationships `Waq ~token [ user2_id ] with
  | [ rel ] ->
      assert (rel.id = user2_id);
      assert rel.following;
      assert (not rel.followed_by);
      Lwt.return_unit
  | _ -> assert false);%lwt

  (* user2: Look up && follow @user1 *)
  let%lwt user1_id, _, _ = lookup `Waq ~token:token' ~username:"user1" () in
  follow `Waq ~token:token' user1_id;%lwt

  (* user1: check relationship *)
  (match%lwt get_relationships `Waq ~token [ user2_id ] with
  | [ rel ] ->
      assert (rel.id = user2_id);
      assert rel.following;
      assert rel.followed_by;
      Lwt.return_unit
  | _ -> assert false);%lwt

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

  Lwt.return_unit
