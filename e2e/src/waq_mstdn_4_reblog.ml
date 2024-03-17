open Common

let f =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup me from mstdn_server_domain *)
  let%lwt aid, _, _ =
    lookup `Mstdn ~token:mstdn_token ~username:"user1" ~domain:waq_server_domain
      ()
  in

  (* Follow me from @mstdn1@mstdn_server_domain *)
  follow `Mstdn ~token:mstdn_token aid;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by user2 *)
  let%lwt waq_token' = fetch_access_token ~username:"user2" in
  let%lwt { id; uri; _ } = post `Waq ~token:waq_token' () in

  (* Reblog by me (user1) *)
  let%lwt _ = reblog `Waq ~token:waq_token ~id in
  Lwt_unix.sleep 2.0;%lwt

  (* Get home timeline of @mstdn1@mstdn_server_domain *)
  let%lwt _ =
    home_timeline `Mstdn ~token:mstdn_token >|= function
    | [ `Assoc l ] ->
        (* Check if the timeline is correct *)
        let reblog_uri =
          l |> List.assoc "reblog" |> expect_assoc |> List.assoc "uri"
          |> expect_string
        in
        assert (uri = reblog_uri)
    | _ -> assert false
  in

  Lwt.return_unit
