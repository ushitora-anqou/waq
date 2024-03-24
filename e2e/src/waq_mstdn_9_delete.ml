open Common2

let f env (a0 : agent) (a1 : agent) =
  (* a0: Follow a1 *)
  follow_agent env a0 a1;
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* a1: Post *)
  let { uri; id = a1_post_id; _ } = post env a1 () in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* a0: Get the post id *)
  let a0_post_id =
    match search env a0 uri with _, [ s ], _ -> s.id | _ -> assert false
  in

  (* a0: Reblog the post *)
  let a0_reblog_id = reblog env a0 ~id:a0_post_id |> fun r -> r.id in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* a0: Check the posts *)
  let _ = get_status env a0 a0_post_id in
  let _ = get_status env a0 a0_reblog_id in

  (* a1: Delete the post *)
  delete_status env a1 a1_post_id |> ignore;
  Eio.Time.sleep (Eio.Stdenv.clock env) 2.0;

  (* a0: Check the posts *)
  expect_no_status env a0 a0_post_id;
  expect_no_status env a0 a0_reblog_id;

  (***************************)

  (* a0: Follow a1 *)
  follow_agent env a1 a0;

  (* a1: Post *)
  let { uri; id = post_id; _ } = post env a1 () in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* a0: Get the post id *)
  let a0_post_id =
    match search env a0 uri with _, [ s ], _ -> s.id | _ -> assert false
  in

  (* a0: Reblog the post *)
  let a0_reblog_id = reblog env a0 ~id:a0_post_id |> fun r -> r.id in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* a0: Check the posts *)
  let _ = get_status env a0 a0_post_id in
  let _ = get_status env a0 a0_reblog_id in

  (* a1: Check if a0 reblogged a0_post_id *)
  let { reblogs_count; _ } = get_status env a1 post_id in
  assert (reblogs_count = 1);

  (* a0: Unreblog the post *)
  unreblog env a0 ~id:a0_post_id |> ignore;
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* a0: Check the posts *)
  let _ = get_status env a0 a0_post_id in
  expect_no_status env a0 a0_reblog_id;

  (*
  (* a1: Check if a0 unreblogged a0_post_id *)
  let { reblogs_count; _ } = get_status a1 post_id in
  assert (reblogs_count = 0);
  *)
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
