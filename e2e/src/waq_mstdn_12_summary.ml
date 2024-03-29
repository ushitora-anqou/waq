open Common2

let f (a0 : agent) (a1 : agent) =
  let spoiler_text = "すぽいらーてきすと" in

  (* a0: Post with summary (spoiler_text) *)
  let%lwt { uri; _ } = post a0 ~spoiler_text () in

  (* a1: Check the post by lookup *)
  let%lwt _, [ s ], _ = search a1 uri in
  assert (s.spoiler_text = spoiler_text);

  Lwt.return_unit
  [@@warning "-8"]

let f_mstdn_waq =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  let a0 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"mstdn1"
      ~domain:mstdn_server_domain
  in
  let a1 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  f a0 a1;%lwt
  Lwt.return_unit

let f_waq_mstdn =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  let a0 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  let a1 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"mstdn1"
      ~domain:mstdn_server_domain
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
  f a0 a1;%lwt
  Lwt.return_unit
