open Common2

let get_preview_card_from_a1 ?(wait_for_card = false) env (a0 : agent) (a1 : agent) content =
  (* a1: Follow a0 *)
  follow_agent env a1 a0;

  (* a0: Post link *)
  let { uri; _ } = post env a0 ~content () in
  Eio.Time.sleep (Eio.Stdenv.clock env) 2.0;

  (* a1: Check the post. The post should have been fetched in advance because a1 follows a0.

     Preview cards are generated asynchronously by crawling the linked page,
     which can take more than the sleep above. When [wait_for_card] is set,
     retry the search until the card shows up. *)
  let rec loop i =
    let _, [ s ], _ = search env a1 uri in
    if (not wait_for_card) || Option.is_some s.card || i = 0 then s
    else (
      Eio.Time.sleep (Eio.Stdenv.clock env) 2.0;
      loop (i - 1))
  in
  loop 10
[@@warning "-8"]

let f_case1 env (a0 : agent) (a1 : agent) =
  (* The page is served by the ogp-server pod deployed in the e2e cluster:
     external sites such as www.youtube.com block crawls from CI runner IPs
     and make this test flaky. *)
  let url = "http://ogp.waq-e2e.anqou.net/" in
  let s = get_preview_card_from_a1 ~wait_for_card:true env a0 a1 url in
  assert (Option.is_some s.card);
  let c = Option.get s.card in
  assert (c.url = url);
  ()

let f_case2 env (a0 : agent) (a1 : agent) =
  let content = "@" ^ acct_of_agent ~from:a0 a1 in
  let s = get_preview_card_from_a1 env a0 a1 content in
  assert (List.length s.mentions = 1);
  assert (Option.is_none s.card);
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
  f_case1 env a0 a1;
  f_case2 env a0 a1;
  ()

let f_waq_waq =
  make_waq_scenario @@ fun env token ->
  let token2 = fetch_access_token env ~username:"user2" in
  let a0 =
    make_agent ~kind:`Waq ~token ~username:"user1" ~domain:waq_server_domain
  in
  let a1 =
    make_agent ~kind:`Waq ~token:token2 ~username:"user2"
      ~domain:waq_server_domain
  in
  f_case1 env a0 a1;
  f_case2 env a0 a1;
  ()
