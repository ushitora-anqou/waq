open Common

let f =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup me from mstdn_server_domain *)
  let%lwt aid, _, _ =
    lookup `Mstdn ~token:mstdn_token ~username:"user1" ~domain:waq_server_domain
      ()
  in
  (* Lookup @mstdn1@mstdn_server_domain *)
  let%lwt mstdn1_id, _username, _acct =
    lookup `Waq ~token:waq_token ~username:"mstdn1" ~domain:mstdn_server_domain
      ()
  in
  (* Follow me from @mstdn1@mstdn_server_domain *)
  follow `Mstdn ~token:mstdn_token aid;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by me *)
  let%lwt { id = waq_status_id; _ } = post `Waq ~token:waq_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get id of the post *)
  let%lwt mstdn_status_id =
    home_timeline `Mstdn ~token:mstdn_token >|= function
    | [ `Assoc l ] -> List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  (* Favourite the post by @mstdn1@mstdn_server_domain *)
  let%lwt _ = fav `Mstdn ~token:mstdn_token ~id:mstdn_status_id in
  Lwt_unix.sleep 1.0;%lwt

  (* Check if the post is favourited *)
  (match%lwt get_favourited_by `Waq ~token:waq_token ~id:waq_status_id with
  | [ a ] ->
      assert (a.id = mstdn1_id);
      Lwt.return_unit
  | _ -> assert false);%lwt

  (* Check notification *)
  (match%lwt get_notifications `Waq ~token:waq_token with
  | [
   {
     typ = "favourite";
     account = { id = account_id; _ };
     status = Some { id = status_id; _ };
     _;
   };
   { typ = "follow"; account = { id = account_id'; _ }; _ };
  ] ->
      assert (account_id = mstdn1_id);
      assert (status_id = waq_status_id);
      assert (account_id' = mstdn1_id);
      Lwt.return_unit
  | _ -> assert false);%lwt

  (* Unfavourite the post *)
  let%lwt _ = unfav `Mstdn ~token:mstdn_token ~id:mstdn_status_id in
  Lwt_unix.sleep 1.0;%lwt

  (* Check if the post is unfavourited *)
  (match%lwt get_favourited_by `Waq ~token:waq_token ~id:waq_status_id with
  | [] -> Lwt.return_unit
  | _ -> assert false);%lwt

  Lwt.return_unit
