open Common2

let f (a0 : agent) (a1 : agent) (a2 : agent) =
  (* a0: Post with mentions *)
  let%lwt { uri; _ } =
    post a0
      ~content:
        (Printf.sprintf "@%s @%s てすと"
           (acct_of_agent ~from:a0 a1)
           (acct_of_agent ~from:a0 a2))
      ()
  in
  Lwt_unix.sleep 2.0;%lwt

  (* a1: Check home timeline, which should be empty *)
  let%lwt [] = home_timeline a1 in

  (* a2: Check home timeline, which should be empty *)
  let%lwt [] = home_timeline a2 in

  (* a1: Check its notification *)
  let%lwt [ n ] = get_notifications a1 in
  assert ((Option.get n.status).uri = uri);
  assert (n.account.acct = acct_of_agent ~from:a1 a0);

  (* a2: Check its notification *)
  let%lwt [ n ] = get_notifications a2 in
  assert ((Option.get n.status).uri = uri);
  assert (n.account.acct = acct_of_agent ~from:a2 a0);

  Lwt.return_unit
  [@@warning "-8"]

let f_waq_mstdn =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  let%lwt token2 = fetch_access_token ~username:"user2" in
  let a0 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  let a1 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"admin"
      ~domain:"localhost:3000"
  in
  let a2 =
    make_agent ~kind:`Waq ~token:token2 ~username:"user2"
      ~domain:waq_server_domain
  in
  f a0 a1 a2;%lwt
  Lwt.return_unit

let f_mstdn_waq =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  let%lwt token2 = fetch_access_token ~username:"user2" in
  let a0 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"admin"
      ~domain:"localhost:3000"
  in
  let a1 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  let a2 =
    make_agent ~kind:`Waq ~token:token2 ~username:"user2"
      ~domain:waq_server_domain
  in
  f a0 a1 a2;%lwt
  Lwt.return_unit

let f_waq_waq =
  make_waq_scenario @@ fun token ->
  let%lwt token2 = fetch_access_token ~username:"user2" in
  let%lwt token3 = fetch_access_token ~username:"user3" in
  let a0 =
    make_agent ~kind:`Waq ~token ~username:"user1" ~domain:waq_server_domain
  in
  let a1 =
    make_agent ~kind:`Waq ~token:token2 ~username:"user2"
      ~domain:waq_server_domain
  in
  let a2 =
    make_agent ~kind:`Waq ~token:token3 ~username:"user3"
      ~domain:waq_server_domain
  in
  f a0 a1 a2
