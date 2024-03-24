open Common

let f =
  make_waq_and_mstdn_scenario @@ fun env waq_token mstdn_token ->
  (* Lookup me from mstdn_server_domain *)
  let aid, _, _ =
    lookup env `Mstdn ~token:mstdn_token ~username:"user1"
      ~domain:waq_server_domain ()
  in
  (* Lookup @mstdn1@mstdn_server_domain *)
  let mstdn1_id, _username, _acct =
    lookup env `Waq ~token:waq_token ~username:"mstdn1"
      ~domain:mstdn_server_domain ()
  in
  (* Follow me from @mstdn1@mstdn_server_domain *)
  follow env `Mstdn ~token:mstdn_token aid;
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Post by me *)
  let ({ id = waq_status_id; _ } : status) =
    post env `Waq ~token:waq_token ()
  in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Get id of the post *)
  let mstdn_status_id =
    home_timeline env `Mstdn ~token:mstdn_token |> function
    | [ `Assoc l ] -> List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  (* Favourite the post by @mstdn1@mstdn_server_domain *)
  let _ = fav env `Mstdn ~token:mstdn_token ~id:mstdn_status_id in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Check if the post is favourited *)
  (match get_favourited_by env `Waq ~token:waq_token ~id:waq_status_id with
  | [ a ] ->
      assert (a.id = mstdn1_id);
      ()
  | _ -> assert false);

  (* Check notification *)
  (match get_notifications env `Waq ~token:waq_token with
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
      ()
  | _ -> assert false);

  (* Unfavourite the post *)
  let _ = unfav env `Mstdn ~token:mstdn_token ~id:mstdn_status_id in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Check if the post is unfavourited *)
  (match get_favourited_by env `Waq ~token:waq_token ~id:waq_status_id with
  | [] -> ()
  | _ -> assert false);

  ()
