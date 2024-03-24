open Common

let f =
  make_waq_scenario @@ fun env token ->
  let user1_id, _, _ = lookup env `Waq ~token ~username:"user1" () in
  let user2_id, _, _ = lookup env `Waq ~token ~username:"user2" () in
  let user3_id, _, _ = lookup env `Waq ~token ~username:"user3" () in
  let token2 = fetch_access_token env ~username:"user2" in
  let token3 = fetch_access_token env ~username:"user3" in

  let ({ id; _ } : status) = post env `Waq ~token () in
  let { favourited; _ } = fav env `Waq ~token ~id in
  assert favourited;
  let { favourited; _ } = fav env `Waq ~token:token2 ~id in
  assert favourited;
  let { favourited; favourites_count; _ } = fav env `Waq ~token:token3 ~id in
  assert favourited;
  assert (favourites_count = 3);

  let l = get_favourited_by env `Waq ~token ~id in
  assert (List.length l = 3);
  assert (
    l |> List.find_opt (fun (a : account) -> a.id = user1_id) |> Option.is_some);
  assert (
    l |> List.find_opt (fun (a : account) -> a.id = user2_id) |> Option.is_some);
  assert (
    l |> List.find_opt (fun (a : account) -> a.id = user3_id) |> Option.is_some);

  (match get_notifications env `Waq ~token with
  | [
   {
     typ = "favourite";
     account = { id = account_id3; _ };
     status = Some { id = status_id3; _ };
     _;
   };
   {
     typ = "favourite";
     account = { id = account_id2; _ };
     status = Some { id = status_id2; _ };
     _;
   };
  ] ->
      assert (account_id3 = user3_id);
      assert (account_id2 = user2_id);
      assert (status_id3 = id);
      assert (status_id2 = id)
  | _ -> assert false);

  let { favourited; _ } = unfav env `Waq ~token ~id in
  assert (not favourited);
  let { favourited; _ } = unfav env `Waq ~token:token2 ~id in
  assert (not favourited);
  let { favourited; favourites_count; _ } = unfav env `Waq ~token:token3 ~id in
  assert (not favourited);
  assert (favourites_count = 0);

  (match get_favourited_by env `Waq ~token ~id with
  | [] -> ()
  | _ -> assert false);

  (match get_notifications env `Waq ~token with [] -> () | _ -> assert false);

  ()
