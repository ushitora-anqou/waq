open Common2

let f (a0 : agent) (a1 : agent) =
  (* a0: Follow a1 *)
  follow_agent a0 a1;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* a1: Post with attachments *)
  let%lwt { id = media_id; _ } =
    upload_media a1 ~filename:"test0.png" ~data:test_image
      ~content_type:"image/png"
  in
  let%lwt { id = media_id2; _ } =
    upload_media a1 ~filename:"test1.png" ~data:test_image
      ~content_type:"image/png"
  in
  let%lwt { uri; media_attachments; _ } =
    post a1 ~media_ids:[ media_id; media_id2 ] ()
  in
  assert (
    media_attachments
    |> List.map (fun (a : media_attachment) -> a.id)
    = [ media_id; media_id2 ]);
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Get the post *)
  let%lwt a0_post =
    match%lwt search a0 uri with
    | _, [ s ], _ -> Lwt.return s
    | _ -> assert false
  in

  (* a0: Check the post *)
  let ats = a0_post.media_attachments in
  assert (List.length ats = 2);
  assert (ats |> List.for_all (fun (a : media_attachment) -> a.type_ = "image"));

  Lwt.return_unit

let f_waq_mstdn =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  let a0 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  let a1 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"admin"
      ~domain:"localhost:3000"
  in
  f a0 a1;%lwt
  Lwt.return_unit

let f_mstdn_waq =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  let a0 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"admin"
      ~domain:"localhost:3000"
  in
  let a1 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  f a0 a1;%lwt
  Lwt.return_unit

let f_waq_waq =
  make_waq_scenario @@ fun token ->
  let%lwt token2 = fetch_access_token ~username:"user2" in
  let a0 =
    make_agent ~kind:`Waq ~token ~username:"user1" ~domain:waq_server_domain
  in
  let a1 =
    make_agent ~kind:`Waq ~token:token2 ~username:"user2"
      ~domain:waq_server_domain
  in
  f a0 a1
