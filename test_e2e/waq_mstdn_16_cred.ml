open Common2

let strip_html_tags s = Soup.(s |> parse |> texts |> String.concat "")

let f (a0 : agent) (a1 : agent) =
  (* a0: Follow a1 *)
  follow_agent a0 a1;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* a1: Update credentials *)
  let modified_display_name = "modified display name" in
  let modified_note = "modified note" in
  let%lwt a =
    update_credentials a1 ~display_name:modified_display_name
      ~note:modified_note ()
  in
  assert (a.display_name = modified_display_name);
  assert (strip_html_tags a.note = modified_note);
  Lwt_unix.sleep 1.0;%lwt

  (* a0: Check a1's info *)
  let%lwt [ a ], _, _ = search a0 (acct_of_agent ~from:a0 a1) in
  assert (a.display_name = modified_display_name);
  assert (strip_html_tags a.note = modified_note);

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
