open Common

let f =
  make_waq_scenario @@ fun token ->
  let%lwt user1_id, _, _ = lookup `Waq ~token ~username:"user1" () in
  let%lwt user2_id, _, _ = lookup `Waq ~token ~username:"user2" () in
  let%lwt user3_id, _, _ = lookup `Waq ~token ~username:"user3" () in
  let%lwt token2 = fetch_access_token ~username:"user2" in
  let%lwt token3 = fetch_access_token ~username:"user3" in

  let%lwt { id; _ } = post `Waq ~token () in
  let%lwt { favourited; _ } = fav `Waq ~token ~id in
  assert favourited;
  let%lwt { favourited; _ } = fav `Waq ~token:token2 ~id in
  assert favourited;
  let%lwt { favourited; favourites_count; _ } = fav `Waq ~token:token3 ~id in
  assert favourited;
  assert (favourites_count = 3);

  let%lwt l = get_favourited_by `Waq ~token ~id in
  assert (List.length l = 3);
  assert (
    l |> List.find_opt (fun (a : account) -> a.id = user1_id) |> Option.is_some);
  assert (
    l |> List.find_opt (fun (a : account) -> a.id = user2_id) |> Option.is_some);
  assert (
    l |> List.find_opt (fun (a : account) -> a.id = user3_id) |> Option.is_some);

  (match%lwt get_notifications `Waq ~token with
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
      assert (status_id2 = id);
      Lwt.return_unit
  | _ -> assert false);%lwt

  let%lwt { favourited; _ } = unfav `Waq ~token ~id in
  assert (not favourited);
  let%lwt { favourited; _ } = unfav `Waq ~token:token2 ~id in
  assert (not favourited);
  let%lwt { favourited; favourites_count; _ } = unfav `Waq ~token:token3 ~id in
  assert (not favourited);
  assert (favourites_count = 0);

  (match%lwt get_favourited_by `Waq ~token ~id with
  | [] -> Lwt.return_unit
  | _ -> assert false);%lwt

  (match%lwt get_notifications `Waq ~token with
  | [] -> Lwt.return_unit
  | _ -> assert false);%lwt

  Lwt.return_unit
