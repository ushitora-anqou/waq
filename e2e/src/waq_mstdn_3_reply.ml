open Common

let f =
  make_waq_and_mstdn_scenario @@ fun env waq_token mstdn_token ->
  (* Lookup me from mstdn_server_domain *)
  let aid, _, _ =
    lookup env `Mstdn ~token:mstdn_token ~username:"user1"
      ~domain:waq_server_domain ()
  in

  (* Lookup @mstdn1@mstdn_server_domain *)
  let mstdn1_id, _username, _acct =
    lookup env `Waq ~token:waq_token ~username:"mstdn1"
      ~domain:mstdn_server_domain ()
  in

  (* Follow @mstdn1@mstdn_server_domain *)
  follow env `Waq ~token:waq_token mstdn1_id;
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Follow me from @mstdn1@mstdn_server_domain *)
  follow env `Mstdn ~token:mstdn_token aid;
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Post by me *)
  let ({ uri; _ } : status) = post env `Waq ~token:waq_token () in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Get home timeline of @mstdn1@mstdn_server_domain and obtain the status's id *)
  let id =
    home_timeline env `Mstdn ~token:mstdn_token |> function
    | [ `Assoc l ] ->
        (* Check if the timeline is correct *)
        assert (uri = (List.assoc "uri" l |> expect_string));
        List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  (* Reply by @mstdn1@mstdn_server_domain *)
  let ({ uri; _ } : status) =
    post env `Mstdn ~token:mstdn_token ~in_reply_to_id:id ()
  in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Get home timeline of me and obtain the reply's id *)
  let id =
    home_timeline env `Waq ~token:waq_token |> function
    | [ `Assoc l; `Assoc l2 ] ->
        assert (uri = (List.assoc "uri" l |> expect_string));
        assert (List.assoc "id" l2 = List.assoc "in_reply_to_id" l);
        assert (
          List.assoc "account" l2 |> expect_assoc |> List.assoc "id"
          = List.assoc "in_reply_to_account_id" l);
        List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  (* Reply by me *)
  let ({ uri; _ } : status) =
    post env `Waq ~token:waq_token ~in_reply_to_id:id ()
  in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Get home timeline of @mstdn1@mstdn_server_domain and check *)
  let _ =
    home_timeline env `Mstdn ~token:mstdn_token |> function
    | [ `Assoc l; `Assoc l2; `Assoc _ ] ->
        (* Check if the timeline is correct *)
        assert (uri = (List.assoc "uri" l |> expect_string));
        assert (List.assoc "id" l2 = List.assoc "in_reply_to_id" l);
        assert (
          List.assoc "account" l2 |> expect_assoc |> List.assoc "id"
          = List.assoc "in_reply_to_account_id" l);
        List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  ()
