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

  (* Check notifications *)
  (match%lwt get_notifications `Waq ~token:waq_token with
  | [ { typ = "follow"; account = a; _ } ] ->
      let%lwt id, _, _ =
        lookup `Waq ~token:waq_token ~username:"mstdn1"
          ~domain:mstdn_server_domain ()
      in
      assert (a.id = id);
      Lwt.return_unit
  | _ -> assert false);%lwt

  (* Post by @mstdn1@mstdn_server_domain *)
  let%lwt { uri; _ } = post `Mstdn ~token:mstdn_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Post by me *)
  let%lwt { uri = uri2; _ } = post `Waq ~token:waq_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @mstdn1@mstdn_server_domain and check *)
  (home_timeline `Mstdn ~token:mstdn_token >|= function
   | [ `Assoc l2; `Assoc l ] ->
       (* Check if the timeline is correct *)
       assert (uri = (List.assoc "uri" l |> expect_string));
       assert (uri2 = (List.assoc "uri" l2 |> expect_string));
       ()
   | _ -> assert false);%lwt

  (* Unfollow me from @mstdn1@mstdn_server_domain *)
  unfollow `Mstdn ~token:mstdn_token aid;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @mstdn1@mstdn_server_domain and check again *)
  (home_timeline `Mstdn ~token:mstdn_token >|= function
   | [ `Assoc l ] ->
       (* Check if the timeline is correct *)
       assert (uri = (List.assoc "uri" l |> expect_string));
       ()
   | _ -> assert false);%lwt

  Lwt.return_unit
