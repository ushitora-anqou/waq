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
  Lwt_unix.sleep 10.0;%lwt

  (* a1: Check home timeline, which should be empty *)
  let%lwt [] = home_timeline a1 in

  (* a2: Check home timeline, which should be empty *)
  let%lwt [] = home_timeline a2 in

  (* a1: Check its notification *)
  let%lwt [ n ] =
    get_notifications a1
    >|= List.filter (fun (n : notification) -> n.typ = "mention")
  in
  assert ((Option.get n.status).uri = uri);
  assert (n.account.acct = acct_of_agent ~from:a1 a0);
  assert (List.length (Option.get n.status).mentions = 2);

  (* a2: Check its notification *)
  let%lwt [ n ] = get_notifications a2 in
  assert ((Option.get n.status).uri = uri);
  assert (n.account.acct = acct_of_agent ~from:a2 a0);
  assert (List.length (Option.get n.status).mentions = 2);

  Lwt.return_unit
  [@@warning "-8"]

let f_waq_mstdn_waq () =
  launch_waq_and_mstdn @@ fun ctxt ->
  let a0 = generate_waq_agent ctxt in
  let a1 = generate_mstdn_agent ctxt in
  let a2 = generate_waq_agent ctxt in
  f a0 a1 a2

let f_mstdn_waq_waq () =
  launch_waq_and_mstdn @@ fun ctxt ->
  let a0 = generate_mstdn_agent ctxt in
  let a1 = generate_waq_agent ctxt in
  let a2 = generate_waq_agent ctxt in
  f a0 a1 a2

let f_waq_waq_waq () =
  launch_waq @@ fun ctxt ->
  let a0 = generate_waq_agent ctxt in
  let a1 = generate_waq_agent ctxt in
  let a2 = generate_waq_agent ctxt in
  f a0 a1 a2

let f_mstdn_waq_mstdn () =
  launch_waq_and_mstdn @@ fun ctxt ->
  let a0 = generate_mstdn_agent ctxt in
  let a1 = generate_waq_agent ctxt in
  let a2 = generate_mstdn_agent ctxt in
  f a0 a1 a2
