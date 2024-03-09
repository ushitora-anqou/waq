open Common

let f =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup me from localhost:3000 *)
  let%lwt aid, _, _ =
    lookup `Mstdn ~token:mstdn_token ~username:"user1" ~domain:waq_server_domain
      ()
  in

  (* Lookup @admin@localhost:3000 *)
  let%lwt admin_id, _username, _acct =
    lookup `Waq ~token:waq_token ~username:"admin" ~domain:"localhost:3000" ()
  in

  (* Follow @admin@localhost:3000 *)
  follow `Waq ~token:waq_token admin_id;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Follow me from @admin@localhost:3000 *)
  follow `Mstdn ~token:mstdn_token aid;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by me *)
  let%lwt { uri; _ } = post `Waq ~token:waq_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 and obtain the status's id *)
  let%lwt id =
    home_timeline `Mstdn ~token:mstdn_token >|= function
    | [ `Assoc l ] ->
        (* Check if the timeline is correct *)
        assert (uri = (List.assoc "uri" l |> expect_string));
        List.assoc "id" l |> expect_string
    | _ -> assert false
  in

  (* Reply by @admin@localhost:3000 *)
  let%lwt { uri; _ } = post `Mstdn ~token:mstdn_token ~in_reply_to_id:id () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of me and obtain the reply's id *)
  let%lwt id =
    home_timeline `Waq ~token:waq_token >|= function
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
  let%lwt { uri; _ } = post `Waq ~token:waq_token ~in_reply_to_id:id () in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 and check *)
  let%lwt _ =
    home_timeline `Mstdn ~token:mstdn_token >|= function
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

  Lwt.return_unit
