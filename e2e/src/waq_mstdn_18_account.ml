open Common2

let f env (a0 : agent) =
  (* Check /api/v1/accounts/:id/statuses *)
  let s0 = post env a0 ~content:"てすと" () in

  let ss = get_account_statuses env a0 s0.account.id in
  assert (List.length ss = 1);
  assert ((List.hd ss).id = s0.id);

  let ss = get_account_statuses env a0 ~pinned:true s0.account.id in
  assert (List.length ss = 0);

  ()

let f_waq =
  make_waq_scenario @@ fun env token ->
  let a0 =
    make_agent ~kind:`Waq ~token ~username:"user1" ~domain:waq_server_domain
  in
  f env a0

let f_mstdn =
  make_mstdn_scenario @@ fun env token ->
  let a0 =
    make_agent ~kind:`Mstdn ~token ~username:"user1" ~domain:mstdn_server_domain
  in
  f env a0
