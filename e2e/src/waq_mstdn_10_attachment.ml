open Common2

let f env (a0 : agent) (a1 : agent) =
  (* a0: Follow a1 *)
  follow_agent env a0 a1;
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* a1: Post with attachments *)
  let ({ id = media_id; _ } : media_attachment) =
    upload_media env a1 ~filename:"test0.png" ~data:test_image_large
      ~content_type:"image/png"
  in
  let ({ id = media_id2; _ } : media_attachment) =
    upload_media env a1 ~filename:"test1.png" ~data:test_image_large
      ~content_type:"image/png"
  in
  let { uri; media_attachments; _ } =
    post env a1 ~media_ids:[ media_id; media_id2 ] ()
  in
  assert (
    media_attachments
    |> List.map (fun (a : media_attachment) -> a.id)
    = [ media_id; media_id2 ]);
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* a0: Get the post *)
  let a0_post =
    match search env a0 uri with _, [ s ], _ -> s | _ -> assert false
  in

  (* a0: Check the post *)
  let ats = a0_post.media_attachments in
  assert (List.length ats = 2);
  assert (ats |> List.for_all (fun (a : media_attachment) -> a.type_ = "image"));

  ()

let f_waq_mstdn =
  make_waq_and_mstdn_scenario @@ fun env waq_token mstdn_token ->
  let a0 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  let a1 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"mstdn1"
      ~domain:mstdn_server_domain
  in
  f env a0 a1;
  ()

let f_mstdn_waq =
  make_waq_and_mstdn_scenario @@ fun env waq_token mstdn_token ->
  let a0 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"mstdn1"
      ~domain:mstdn_server_domain
  in
  let a1 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  f env a0 a1;
  ()

let f_waq_waq =
  make_waq_scenario @@ fun env token ->
  let token2 = fetch_access_token env ~username:"user2" in
  let a0 =
    make_agent ~kind:`Waq ~token ~username:"user1" ~domain:waq_server_domain
  in
  let a1 =
    make_agent ~kind:`Waq ~token:token2 ~username:"user2"
      ~domain:waq_server_domain
  in
  f env a0 a1
