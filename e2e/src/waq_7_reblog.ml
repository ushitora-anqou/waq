open Common

let f =
  make_waq_scenario @@ fun env token ->
  let user2_id, _, _ = lookup env `Waq ~token ~username:"user2" () in
  let token2 = fetch_access_token env ~username:"user2" in
  let token3 = fetch_access_token env ~username:"user3" in
  let ({ id; _ } : status) = post env `Waq ~token () in
  let _ = reblog env `Waq ~token ~id in
  let _ = reblog env `Waq ~token:token2 ~id in

  let ntfs = get_notifications env `Waq ~token in
  (match ntfs with
  | [
   {
     typ = "reblog";
     account = { id = account_id2; _ };
     status = Some { id = status_id2; reblogs_count; _ };
     _;
   };
  ] ->
      assert (account_id2 = user2_id);
      assert (status_id2 = id);
      assert (reblogs_count = 2);
      ()
  | _ -> assert false);

  (* Wrong unreblog *)
  expect_exc (fun () -> unreblog env `Waq ~token:token3 ~id);
  let { reblogs_count; _ } = get_status env `Waq ~token id in
  assert (reblogs_count = 2);

  (* Actual unreblogs *)
  let { id = unreblog_id; reblogs_count; reblogged; _ } =
    unreblog env `Waq ~token:token2 ~id
  in
  assert (unreblog_id = id);
  assert (reblogs_count = 1);
  assert (reblogged = Some false);
  let { id = unreblog_id; reblogs_count; reblogged; _ } =
    unreblog env `Waq ~token ~id
  in
  assert (unreblog_id = id);
  assert (reblogs_count = 0);
  assert (reblogged = Some false);

  ()
