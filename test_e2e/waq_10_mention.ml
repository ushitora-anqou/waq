open Common

let f =
  make_waq_scenario @@ fun token ->
  let%lwt user1_id, _, _ = lookup `Waq ~token ~username:"user1" () in
  let%lwt token2 = fetch_access_token ~username:"user2" in
  let%lwt { id; _ } = post `Waq ~token ~content:"@user2 てすと" () in

  let%lwt ntfs = get_notifications `Waq ~token:token2 in
  (match ntfs with
  | [
   {
     typ = "mention";
     account = { id = account_id; _ };
     status = Some { id = status_id; _ };
     _;
   };
  ] ->
      assert (account_id = user1_id);
      assert (status_id = id);
      Lwt.return_unit
  | _ -> assert false);%lwt

  Lwt.return_unit
