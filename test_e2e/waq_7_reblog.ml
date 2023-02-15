open Common

let f =
  make_waq_scenario @@ fun token ->
  let%lwt user2_id, _, _ = lookup `Waq ~token ~username:"user2" () in
  let%lwt token' = fetch_access_token ~username:"user2" in
  let%lwt { id; _ } = post `Waq ~token () in
  let%lwt { id = reblog_id; _ } = reblog `Waq ~token:token' ~id in

  let%lwt ntfs = get_notifications `Waq ~token in
  match ntfs with
  | [
   {
     typ = "reblog";
     account = { id = account_id; _ };
     status = Some { id = status_id; _ };
     _;
   };
  ]
    when account_id = user2_id && status_id = reblog_id ->
      Lwt.return_unit
  | _ -> assert false
