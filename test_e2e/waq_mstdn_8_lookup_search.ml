open Common

let f =
  make_waq_and_mstdn_scenario @@ fun waq_token _mstdn_token ->
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
  assert (res1 = res2);
  assert (res1 = res3);

  (* Lookup me *)
  let%lwt res1 = lookup `Waq ~token:waq_token ~username:"user1" () in
  let%lwt res2 =
    lookup_via_v1_accounts_search `Waq ~token:waq_token ~username:"user1" ()
  in
  let%lwt res3 =
    lookup_via_v1_accounts_lookup `Waq ~token:waq_token ~username:"user1" ()
  in
  assert (res1 = res2);
  assert (res1 = res3);

  Lwt.return_unit
