open Common2

let f (a0 : agent) (a1 : agent) =
  (* a0: Follow a1 *)
  follow_agent a0 a1;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* a1: Post *)
  let%lwt { uri; id = a1_post_id; _ } = post a1 () in
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Get the post id *)
  let%lwt a0_post_id =
    match%lwt search a0 uri with
    | _, [ s ], _ -> Lwt.return s.id
    | _ -> assert false
  in

  (* a0: Reblog the post *)
  let%lwt a0_reblog_id = reblog a0 ~id:a0_post_id >|= fun r -> r.id in
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Check the posts *)
  let%lwt _ = get_status a0 a0_post_id in
  let%lwt _ = get_status a0 a0_reblog_id in

  (* a1: Delete the post *)
  delete_status a1 a1_post_id |> ignore_lwt;%lwt
  Lwt_unix.sleep 2.0;%lwt

  (* a0: Check the posts *)
  expect_no_status a0 a0_post_id;%lwt
  expect_no_status a0 a0_reblog_id;%lwt

  (***************************)

  (* a0: Follow a1 *)
  follow_agent a1 a0;%lwt

  (* a1: Post *)
  let%lwt { uri; id = post_id; _ } = post a1 () in
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Get the post id *)
  let%lwt a0_post_id =
    match%lwt search a0 uri with
    | _, [ s ], _ -> Lwt.return s.id
    | _ -> assert false
  in

  (* a0: Reblog the post *)
  let%lwt a0_reblog_id = reblog a0 ~id:a0_post_id >|= fun r -> r.id in
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Check the posts *)
  let%lwt _ = get_status a0 a0_post_id in
  let%lwt _ = get_status a0 a0_reblog_id in

  (* a1: Check if a0 reblogged a0_post_id *)
  let%lwt { reblogs_count; _ } = get_status a1 post_id in
  assert (reblogs_count = 1);

  (* a0: Unreblog the post *)
  unreblog a0 ~id:a0_post_id |> ignore_lwt;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Check the posts *)
  let%lwt _ = get_status a0 a0_post_id in
  expect_no_status a0 a0_reblog_id;%lwt

  (*
  (* a1: Check if a0 unreblogged a0_post_id *)
  let%lwt { reblogs_count; _ } = get_status a1 post_id in
  assert (reblogs_count = 0);
  *)
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
