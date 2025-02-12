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
  let s1 = get_status env a0 s0.id in
  check s1;

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
