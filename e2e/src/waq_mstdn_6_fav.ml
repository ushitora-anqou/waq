open Common

let f =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup @mstdn1@mstdn_server_domain *)
  let%lwt mstdn1_id, _username, _acct =
    lookup `Waq ~token:waq_token ~username:"mstdn1" ~domain:mstdn_server_domain
      ()
  in
  (* Follow @mstdn1@mstdn_server_domain *)
  follow `Waq ~token:waq_token mstdn1_id;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Get user1's id on mstdn_server_domain *)
  let%lwt mstdn_user1_id, _, _ =
    lookup `Mstdn ~token:mstdn_token ~username:"user1" ()
  in

  (* Post by @mstdn1@mstdn_server_domain *)
  let%lwt { id = mstdn_post_id; _ } = post `Mstdn ~token:mstdn_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get id of the post *)
  let%lwt id =
    home_timeline `Waq ~token:waq_token >|= function
    | [ `Assoc l ] -> List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  (* Favourite the post by me *)
  let%lwt s = fav `Waq ~token:waq_token ~id in
  Lwt_unix.sleep 1.0;%lwt
  assert s.favourited;
  let%lwt s = get_status `Waq ~token:waq_token id in
  assert s.favourited;

  (* Check if the post is favourited in mstdn_server_domain *)
  (match%lwt get_favourited_by `Mstdn ~token:mstdn_token ~id:mstdn_post_id with
  | [ a ] ->
      assert (a.id = mstdn_user1_id);
      Lwt.return_unit
  | _ -> assert false);%lwt

  (* Unfavourite the post *)
  let%lwt s = unfav `Waq ~token:waq_token ~id in
  Lwt_unix.sleep 1.0;%lwt
  assert (not s.favourited);
  let%lwt s = get_status `Waq ~token:waq_token id in
  assert (not s.favourited);

  (* Check if the post is unfavourited *)
  (match%lwt get_favourited_by `Mstdn ~token:mstdn_token ~id:mstdn_post_id with
  | [] -> Lwt.return_unit
  | _ -> assert false);%lwt

  Lwt.return_unit
