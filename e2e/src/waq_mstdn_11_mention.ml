open Common2

let f env (a0 : agent) (a1 : agent) (a2 : agent) =
  (* a0: Post with mentions *)
  let { uri; _ } =
    post env a0
      ~content:
        (Printf.sprintf "@%s @%s てすと"
           (acct_of_agent ~from:a0 a1)
           (acct_of_agent ~from:a0 a2))
      ()
  in
  Eio.Time.sleep (Eio.Stdenv.clock env) 10.0;

  (* a1: Check home timeline, which should be empty *)
  let [] = home_timeline env a1 in

  (* a2: Check home timeline, which should be empty *)
  let [] = home_timeline env a2 in

  (* a1: Check its notification *)
  let [ n ] =
    get_notifications env a1
    |> List.filter (fun (n : notification) -> n.typ = "mention")
  in
  assert ((Option.get n.status).uri = uri);
  assert (n.account.acct = acct_of_agent ~from:a1 a0);
  assert (List.length (Option.get n.status).mentions = 2);

  (* a2: Check its notification *)
  let [ n ] = get_notifications env a2 in
  assert ((Option.get n.status).uri = uri);
  assert (n.account.acct = acct_of_agent ~from:a2 a0);
  assert (List.length (Option.get n.status).mentions = 2);

  ()
[@@warning "-8"]

let f_waq_mstdn_waq () =
  launch_waq_and_mstdn @@ fun env ctxt ->
  let a0 = generate_waq_agent ctxt in
  let a1 = generate_mstdn_agent ctxt in
  let a2 = generate_waq_agent ctxt in
  f env a0 a1 a2

let f_mstdn_waq_waq () =
  launch_waq_and_mstdn @@ fun env ctxt ->
  let a0 = generate_mstdn_agent ctxt in
  let a1 = generate_waq_agent ctxt in
  let a2 = generate_waq_agent ctxt in
  f env a0 a1 a2

let f_waq_waq_waq () =
  launch_waq @@ fun env ctxt ->
  let a0 = generate_waq_agent ctxt in
  let a1 = generate_waq_agent ctxt in
  let a2 = generate_waq_agent ctxt in
  f env a0 a1 a2

let f_mstdn_waq_mstdn () =
  launch_waq_and_mstdn @@ fun env ctxt ->
  let a0 = generate_mstdn_agent ctxt in
  let a1 = generate_waq_agent ctxt in
  let a2 = generate_mstdn_agent ctxt in
  f env a0 a1 a2
