open Common2

let strip_html_tags s = Soup.(s |> parse |> texts |> String.concat "")

let f (a0 : agent) (a1 : agent) =
  (* a0: Follow a1 *)
  follow_agent a0 a1;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* a1: Update display name *)
  let modified_display_name = "modified display name" in
  let%lwt a = update_credentials a1 ~display_name:modified_display_name () in
  assert (a.display_name = modified_display_name);

  (* a1: Update credentials *)
  assert (not a.bot);
  let%lwt old_avatar_url, old_header_url =
    search a0 (acct_of_agent ~from:a0 a1) >|= fun ([ a ], _, _) ->
    (a.avatar, a.header)
  in
  let modified_note = "modified note" in
  let modified_avatar = test_image in
  let modified_header = test_image in
  let modified_bot = true in
  let%lwt a =
    update_credentials a1 ~note:modified_note ~avatar:modified_avatar
      ~header:modified_header ~bot:modified_bot ()
  in
  assert (a.display_name = modified_display_name);
  assert (strip_html_tags a.note = modified_note);
  assert a.bot;
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Check a1's info *)
  let%lwt [ a ], _, _ = search a0 (acct_of_agent ~from:a0 a1) in
  assert (a.display_name = modified_display_name);
  assert (strip_html_tags a.note = modified_note);
  assert (a.avatar <> old_avatar_url);
  assert (a.header <> old_header_url);
  assert a.bot;

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
