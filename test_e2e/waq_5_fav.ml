open Common

let f =
  make_waq_scenario @@ fun token ->
  let%lwt { id; account; _ } = post `Waq ~token () in
  let%lwt { favourited; _ } = fav `Waq ~token ~id in
  assert favourited;
  (match%lwt get_favourited_by `Waq ~token ~id with
  | [ a ] ->
      assert (a.id = account.id);
      Lwt.return_unit
  | _ -> assert false);%lwt
  let%lwt { favourited; _ } = unfav `Waq ~token ~id in
  assert (not favourited);
  (match%lwt get_favourited_by `Waq ~token ~id with
  | [] -> Lwt.return_unit
  | _ -> assert false);%lwt
  Lwt.return_unit
