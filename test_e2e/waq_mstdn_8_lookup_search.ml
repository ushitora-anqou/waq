open Common

let f =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup @admin@localhost:3000 *)
  let%lwt res1 =
    lookup `Waq ~token:waq_token ~username:"admin" ~domain:"localhost:3000" ()
  in
  let%lwt res2 =
    lookup_via_v1_accounts_search `Waq ~token:waq_token ~username:"admin"
      ~domain:"localhost:3000" ()
  in
  let%lwt res3 =
    lookup_via_v1_accounts_lookup `Waq ~token:waq_token ~username:"admin"
      ~domain:"localhost:3000" ()
  in
  let%lwt res4 =
    search `Waq ~token:waq_token "http://localhost:3000/users/admin"
  in
  assert (res1 = res2);
  assert (res1 = res3);
  assert (
    match res4 with
    | [ acct ], _, _ when res1 = (acct.id, acct.username, acct.acct) -> true
    | _ -> false);

  (* Lookup me *)
  let%lwt res1 = lookup `Waq ~token:waq_token ~username:"user1" () in
  let%lwt res2 =
    lookup_via_v1_accounts_search `Waq ~token:waq_token ~username:"user1" ()
  in
  let%lwt res3 =
    lookup_via_v1_accounts_lookup `Waq ~token:waq_token ~username:"user1" ()
  in
  let%lwt res4 =
    search `Waq ~token:waq_token (waq_server_name ^/ "users/user1")
  in
  assert (res1 = res2);
  assert (res1 = res3);
  assert (
    match res4 with
    | [ acct ], _, _ when res1 = (acct.id, acct.username, acct.acct) -> true
    | _ -> false);

  (* Lookup post of @admin@localhost:3000 *)
  let%lwt { uri; _ } = post `Mstdn ~token:mstdn_token () in
  let%lwt res = search `Waq ~token:waq_token uri in
  assert (
    match res with _, [ status ], _ when status.uri = uri -> true | _ -> false);

  Lwt.return_unit
