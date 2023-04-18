open Common

let f =
  make_waq_scenario @@ fun token ->
  let%lwt user2_id, _, _ = lookup `Waq ~token ~username:"user2" () in
  let%lwt token2 = fetch_access_token ~username:"user2" in
  let%lwt token3 = fetch_access_token ~username:"user3" in
  let%lwt { id; _ } = post `Waq ~token () in
  let%lwt _ = reblog `Waq ~token ~id in
  let%lwt { id = reblog_id2; _ } = reblog `Waq ~token:token2 ~id in

  let%lwt ntfs = get_notifications `Waq ~token in
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
      assert (status_id2 = reblog_id2);
      assert (reblogs_count = 2);
      Lwt.return_unit
  | _ -> assert false);%lwt

  (* Wrong unreblog *)
  (try%lwt
     unreblog `Waq ~token:token3 ~id |> ignore_lwt;%lwt
     assert false
   with _ -> Lwt.return_unit);%lwt
  let%lwt { reblogs_count; _ } = get_status `Waq ~token id in
  assert (reblogs_count = 2);

  (* Actual unreblogs *)
  let%lwt { id = unreblog_id; reblogs_count; reblogged; _ } =
    unreblog `Waq ~token:token2 ~id
  in
  assert (unreblog_id = id);
  assert (reblogs_count = 1);
  assert (not reblogged);
  let%lwt { id = unreblog_id; reblogs_count; reblogged; _ } =
    unreblog `Waq ~token ~id
  in
  assert (unreblog_id = id);
  assert (reblogs_count = 0);
  assert (not reblogged);

  Lwt.return_unit
