open Common2

let f env (a0 : agent) =
  let check (s : status) =
    assert (s.reblog = None);
    assert (s.reblogged = Some false);
    assert (s.favourited = Some false);
    assert (s.pinned = Some false);
    assert (s.bookmarked = Some false);
    assert (s.muted = Some false);
    assert (
      match s.content with
      | None -> false
      | Some s -> (
          try
            Pcre.(exec_all ~rex:(regexp "てすと") s) |> ignore;
            true
          with Not_found -> false));
    ()
  in

  (* a0: Post *)
  let s0 = post env a0 ~content:"てすと" () in
  check s0;

  (* a0: Get the post *)
  let s0' = get_status env a0 s0.id in
  check s0';

  (* test context *)
  let s00 = post env a0 ~content:"てすと00" () in
  let s01 = post env a0 ~content:"てすと01" ~in_reply_to_id:s00.id () in
  let s02 = post env a0 ~content:"てすと02" ~in_reply_to_id:s01.id () in
  let s03 = post env a0 ~content:"てすと03" ~in_reply_to_id:s02.id () in
  let s04 = post env a0 ~content:"てすと04" ~in_reply_to_id:s03.id () in
  let s05 = post env a0 ~content:"てすと05" ~in_reply_to_id:s04.id () in
  let s06 = post env a0 ~content:"てすと06" ~in_reply_to_id:s05.id () in
  let s14 = post env a0 ~content:"てすと14" ~in_reply_to_id:s03.id () in
  let s15 = post env a0 ~content:"てすと15" ~in_reply_to_id:s14.id () in
  let s16 = post env a0 ~content:"てすと16" ~in_reply_to_id:s15.id () in
  let ancestors, descendants = get_status_context env a0 s03.id in
  assert (
    List.map (fun (s : status) -> s.id) ancestors = [ s00.id; s01.id; s02.id ]);
  assert (
    List.map (fun (s : status) -> s.id) descendants
    = [ s04.id; s05.id; s06.id; s14.id; s15.id; s16.id ]);

  (* check status visibility *)
  let s = post env a0 ~visibility:"public" ~content:"てすと public" () in
  let _ = get_status env a0 s.id in
  let s = post env a0 ~visibility:"unlisted" ~content:"てすと unlisted" () in
  let _ = get_status env a0 s.id in
  let s = post env a0 ~visibility:"private" ~content:"てすと private" () in
  let _ = get_status env a0 s.id in
  let s = post env a0 ~visibility:"direct" ~content:"てすと direct" () in
  let _ = get_status env a0 s.id in
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
