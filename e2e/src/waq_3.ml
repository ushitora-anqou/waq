open Common

let f =
  make_waq_scenario @@ fun env waq_token ->
  let waq_token' = fetch_access_token env ~username:"user2" in
  let user1_id, _, _ = lookup env `Waq ~token:waq_token ~username:"user1" () in
  let user2_id, _, _ = lookup env `Waq ~token:waq_token ~username:"user2" () in

  (* Follow @user2 *)
  follow env `Waq ~token:waq_token user2_id;
  Eio.Time.sleep env#clock 1.0;

  (* Post by @user2 *)
  let { uri; id; _ } = post env `Waq ~token:waq_token' () in

  (* check accounts *)
  let a = get_account env `Waq user2_id in
  assert (a.statuses_count = 1);

  (* Reply by me *)
  let { uri = uri2; id = id2; _ } =
    post env `Waq ~token:waq_token ~in_reply_to_id:id ()
  in

  (* check accounts *)
  let a = get_account env `Waq user1_id in
  assert (a.statuses_count = 1);

  (* Reply again *)
  let { uri = uri3; _ } =
    post env `Waq ~token:waq_token ~in_reply_to_id:id2 ()
  in

  (* Get my home timeline and check *)
  (match home_timeline env `Waq ~token:waq_token with
  | [ `Assoc l3; `Assoc l2; `Assoc l ] ->
      (* Check if the timeline is correct *)
      assert (uri = (l |> List.assoc "uri" |> expect_string));
      assert (id = (l2 |> List.assoc "in_reply_to_id" |> expect_string));
      assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
      assert (id2 = (l3 |> List.assoc "in_reply_to_id" |> expect_string));
      assert (uri3 = (l3 |> List.assoc "uri" |> expect_string));
      ()
  | _ -> assert false);

  (* Unfollow @user2 *)
  unfollow env `Waq ~token:waq_token user2_id;
  Eio.Time.sleep env#clock 1.0;

  (* Get my home timeline and check again *)
  (match home_timeline env `Waq ~token:waq_token with
  | [ `Assoc l3; `Assoc l2 ] ->
      (* Check if the timeline is correct *)
      assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
      assert (uri3 = (l3 |> List.assoc "uri" |> expect_string));
      ()
  | _ -> assert false);

  (* Check status itself *)
  let s = get_status env `Waq id in
  assert (s.uri = uri);

  (* Check the status's context *)
  let ancestors, descendants = get_status_context env `Waq id2 in
  assert (ancestors |> List.map (fun r -> r.uri) = [ uri ]);
  assert (descendants |> List.map (fun r -> r.uri) = [ uri3 ]);

  (* Check account's statuses *)
  let statuses = get_account_statuses env `Waq user1_id in
  assert ([ uri3; uri2 ] = (statuses |> List.map (fun s -> s.uri)));
  let statuses = get_account_statuses env `Waq ~exclude_replies:true user1_id in
  assert (statuses = []);
  let statuses = get_account_statuses env `Waq user2_id in
  assert ([ uri ] = (statuses |> List.map (fun s -> s.uri)));
  let statuses = get_account_statuses env `Waq ~exclude_replies:true user2_id in
  assert ([ uri ] = (statuses |> List.map (fun s -> s.uri)));

  ()
