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
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"admin"
      ~domain:"localhost:3000"
  in
  let a1 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  f a0 a1;%lwt
  Lwt.return_unit
