open Common2

let f (a0 : agent) (a1 : agent) =
  let url = "https://www.youtube.com/watch?v=OMv_EPMED8Y" in

  (* a1: Follow a0 *)
  follow_agent a1 a0;%lwt

  (* a0: Post link *)
  let%lwt { uri; _ } = post a0 ~content:url () in
  Lwt_unix.sleep 1.0;%lwt

  (* a1: Check the post. The post should have been fetched in advance because a1 follows a0. *)
  let%lwt _, [ s ], _ = search a1 uri in
  assert (List.length Soup.(parse s.content $$ "a" |> to_list) = 1);

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
