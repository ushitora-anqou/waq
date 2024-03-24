open Common

let f =
  make_waq_scenario @@ fun env token ->
  let user1_id, _, _ = lookup env `Waq ~token ~username:"user1" () in
  let token2 = fetch_access_token env ~username:"user2" in
  let ({ id; _ } : status) = post env `Waq ~token ~content:"@user2 てすと" () in

  let ntfs = get_notifications env `Waq ~token:token2 in
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
      ()
  | _ -> assert false);

  (* Handle invalid mentions correctly *)
  let _ = post env `Waq ~token ~content:"@not_found_user test" () in
  home_timeline env `Waq ~token |> ignore;

  ()
