open Common2

let get_preview_card_from_a1 (a0 : agent) (a1 : agent) content =
  (* a1: Follow a0 *)
  follow_agent a1 a0;%lwt

  (* a0: Post link *)
  let%lwt { uri; _ } = post a0 ~content () in
  Lwt_unix.sleep 2.0;%lwt

  (* a1: Check the post. The post should have been fetched in advance because a1 follows a0. *)
  let%lwt _, [ s ], _ = search a1 uri in
  Lwt.return s
  [@@warning "-8"]

let f_case1 (a0 : agent) (a1 : agent) =
  let url = "https://www.youtube.com/watch?v=OMv_EPMED8Y" in
  let%lwt s = get_preview_card_from_a1 a0 a1 url in
  assert (Option.is_some s.card);
  let c = Option.get s.card in
  assert (c.url = url);
  Lwt.return_unit

let f_case2 (a0 : agent) (a1 : agent) =
  let content = "@" ^ acct_of_agent ~from:a0 a1 in
  let%lwt s = get_preview_card_from_a1 a0 a1 content in
  assert (List.length s.mentions = 1);
  assert (Option.is_none s.card);
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
  f_case1 a0 a1;%lwt
  f_case2 a0 a1;%lwt
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
  f_case1 a0 a1;%lwt
  f_case2 a0 a1;%lwt
  Lwt.return_unit
