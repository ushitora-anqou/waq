open Common

let f =
  make_waq_scenario @@ fun token ->
  let%lwt user2_id, _, _ = lookup `Waq ~token ~username:"user2" () in
  let%lwt user3_id, _, _ = lookup `Waq ~token ~username:"user3" () in
  let%lwt token2 = fetch_access_token ~username:"user2" in
  let%lwt token3 = fetch_access_token ~username:"user3" in
  let%lwt { id; _ } = post `Waq ~token () in
  let%lwt _ = reblog `Waq ~token ~id in
  let%lwt { id = reblog_id2; _ } = reblog `Waq ~token:token2 ~id in
  let%lwt { id = reblog_id3; _ } = reblog `Waq ~token:token3 ~id in

  let%lwt ntfs = get_notifications `Waq ~token in
  match ntfs with
  | [
   {
     typ = "reblog";
     account = { id = account_id3; _ };
     status = Some { id = status_id3; reblogs_count; _ };
     _;
   };
   {
     typ = "reblog";
     account = { id = account_id2; _ };
     status = Some { id = status_id2; _ };
     _;
   };
  ] ->
      assert (account_id2 = user2_id);
      assert (status_id2 = reblog_id2);
      assert (account_id3 = user3_id);
      assert (status_id3 = reblog_id3);
      assert (reblogs_count = 3);
      Lwt.return_unit
  | _ -> assert false
