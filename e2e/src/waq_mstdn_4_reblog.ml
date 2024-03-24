open Common

let f =
  make_waq_and_mstdn_scenario @@ fun env waq_token mstdn_token ->
  (* Lookup me from mstdn_server_domain *)
  let aid, _, _ =
    lookup env `Mstdn ~token:mstdn_token ~username:"user1"
      ~domain:waq_server_domain ()
  in

  (* Follow me from @mstdn1@mstdn_server_domain *)
  follow env `Mstdn ~token:mstdn_token aid;
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Post by user2 *)
  let waq_token' = fetch_access_token env ~username:"user2" in
  let ({ id; uri; _ } : status) = post env `Waq ~token:waq_token' () in

  (* Reblog by me (user1) *)
  let _ = reblog env `Waq ~token:waq_token ~id in
  Eio.Time.sleep (Eio.Stdenv.clock env) 2.0;

  (* Get home timeline of @mstdn1@mstdn_server_domain *)
  let _ =
    home_timeline env `Mstdn ~token:mstdn_token |> function
    | [ `Assoc l ] ->
        (* Check if the timeline is correct *)
        let reblog_uri =
          l |> List.assoc "reblog" |> expect_assoc |> List.assoc "uri"
          |> expect_string
        in
        assert (uri = reblog_uri)
    | _ -> assert false
  in

  ()
