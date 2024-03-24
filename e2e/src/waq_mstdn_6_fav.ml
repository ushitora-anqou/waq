open Common

let f =
  make_waq_and_mstdn_scenario @@ fun env waq_token mstdn_token ->
  (* Lookup @mstdn1@mstdn_server_domain *)
  let mstdn1_id, _username, _acct =
    lookup env `Waq ~token:waq_token ~username:"mstdn1"
      ~domain:mstdn_server_domain ()
  in
  (* Follow @mstdn1@mstdn_server_domain *)
  follow env `Waq ~token:waq_token mstdn1_id;
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Get user1's id on mstdn_server_domain *)
  let mstdn_user1_id, _, _ =
    lookup env `Mstdn ~token:mstdn_token ~username:"user1" ()
  in

  (* Post by @mstdn1@mstdn_server_domain *)
  let ({ id = mstdn_post_id; _ } : status) =
    post env `Mstdn ~token:mstdn_token ()
  in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Get id of the post *)
  let id =
    home_timeline env `Waq ~token:waq_token |> function
    | [ `Assoc l ] -> List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  (* Favourite the post by me *)
  let s = fav env `Waq ~token:waq_token ~id in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;
  assert s.favourited;
  let s = get_status env `Waq ~token:waq_token id in
  assert s.favourited;

  (* Check if the post is favourited in mstdn_server_domain *)
  (match get_favourited_by env `Mstdn ~token:mstdn_token ~id:mstdn_post_id with
  | [ a ] ->
      assert (a.id = mstdn_user1_id);
      ()
  | _ -> assert false);

  (* Unfavourite the post *)
  let s = unfav env `Waq ~token:waq_token ~id in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;
  assert (not s.favourited);
  let s = get_status env `Waq ~token:waq_token id in
  assert (not s.favourited);

  (* Check if the post is unfavourited *)
  (match get_favourited_by env `Mstdn ~token:mstdn_token ~id:mstdn_post_id with
  | [] -> ()
  | _ -> assert false);

  ()
