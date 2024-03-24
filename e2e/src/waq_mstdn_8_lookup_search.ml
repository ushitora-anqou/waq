open Common

let f =
  make_waq_and_mstdn_scenario @@ fun env waq_token mstdn_token ->
  (* Lookup @mstdn1@mstdn_server_domain *)
  let res1 =
    lookup env `Waq ~token:waq_token ~username:"mstdn1"
      ~domain:mstdn_server_domain ()
  in
  let res2 =
    lookup_via_v1_accounts_search env `Waq ~token:waq_token ~username:"mstdn1"
      ~domain:mstdn_server_domain ()
  in
  let res3 =
    lookup_via_v1_accounts_lookup env `Waq ~token:waq_token ~username:"mstdn1"
      ~domain:mstdn_server_domain ()
  in
  let res4 = search env `Waq ~token:waq_token (mstdn "/users/mstdn1") in
  assert (res1 = res2);
  assert (res1 = res3);
  assert (
    match res4 with
    | [ acct ], _, _ when res1 = (acct.id, acct.username, acct.acct) -> true
    | _ -> false);

  (* No token should cause an error *)
  expect_exc (fun () ->
      lookup_via_v1_accounts_search env `Waq ~username:"mstdn1"
        ~domain:mstdn_server_domain ());

  (* Lookup me *)
  let res1 = lookup env `Waq ~token:waq_token ~username:"user1" () in
  let res2 =
    lookup_via_v1_accounts_search env `Waq ~token:waq_token ~username:"user1" ()
  in
  let res3 =
    lookup_via_v1_accounts_lookup env `Waq ~token:waq_token ~username:"user1" ()
  in
  let res4 =
    search env `Waq ~token:waq_token (waq_server_name ^/ "users/user1")
  in
  assert (res1 = res2);
  assert (res1 = res3);
  assert (
    match res4 with
    | [ acct ], _, _ when res1 = (acct.id, acct.username, acct.acct) -> true
    | _ -> false);

  (* Lookup post of @mstdn1@mstdn_server_domain *)
  let { uri; _ } = post env `Mstdn ~token:mstdn_token () in
  let res = search env `Waq ~token:waq_token uri in
  assert (
    match res with _, [ status ], _ when status.uri = uri -> true | _ -> false);

  ()
