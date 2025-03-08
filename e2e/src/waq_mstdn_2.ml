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

  (* Check notifications *)
  (match get_notifications env `Waq ~token:waq_token with
  | [ { typ = "follow"; account = a; _ } ] ->
      let id, _, _ =
        lookup env `Waq ~token:waq_token ~username:"mstdn1"
          ~domain:mstdn_server_domain ()
      in
      assert (a.id = id);
      ()
  | _ -> assert false);

  (* Post by @mstdn1@mstdn_server_domain *)
  let ({ uri; _ } : status) = post env `Mstdn ~token:mstdn_token () in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Post by me *)
  let ({ uri = uri2; _ } : status) = post env `Waq ~token:waq_token () in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Get home timeline of @mstdn1@mstdn_server_domain and check *)
  ( home_timeline env `Mstdn ~token:mstdn_token |> function
    | [ `Assoc l2; `Assoc l ] ->
        (* Check if the timeline is correct *)
        assert (uri = (List.assoc "uri" l |> expect_string));
        assert (uri2 = (List.assoc "uri" l2 |> expect_string));
        ()
    | _ -> assert false );

  (* Unfollow me from @mstdn1@mstdn_server_domain *)
  unfollow env `Mstdn ~token:mstdn_token aid;
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Get home timeline of @mstdn1@mstdn_server_domain and check again *)
  ( home_timeline env `Mstdn ~token:mstdn_token |> function
    | [ `Assoc l ] ->
        (* Check if the timeline is correct *)
        assert (uri = (List.assoc "uri" l |> expect_string));
        ()
    | _ -> assert false );

  ()
