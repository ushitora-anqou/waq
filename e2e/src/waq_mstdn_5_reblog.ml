open Common

let f =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup @mstdn1@mstdn_server_domain *)
  let%lwt mstdn1_id, _username, _acct =
    lookup `Waq ~token:waq_token ~username:"mstdn1" ~domain:mstdn_server_domain
      ()
  in

  (* Follow @mstdn1@mstdn_server_domain *)
  follow `Waq ~token:waq_token mstdn1_id;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by user1 *)
  let%lwt { uri; id = post_id; _ } = post `Waq ~token:waq_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Reblog the post by @mstdn1@locahost:3000 *)
  (match%lwt search `Mstdn ~token:mstdn_token uri with
  | _, [ status ], _ ->
      reblog `Mstdn ~token:mstdn_token ~id:status.id |> ignore_lwt
  | _ -> assert false);%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Check home timeline *)
  (home_timeline `Waq ~token:waq_token >|= function
   | [ `Assoc l1; `Assoc l2 ] ->
       (* Check if the timeline is correct *)
       assert (uri = (l2 |> List.assoc "uri" |> expect_string));
       assert (
         uri
         = (l1 |> List.assoc "reblog" |> expect_assoc |> List.assoc "uri"
          |> expect_string));
       ()
   | _ -> assert false);%lwt

  (* Check notifications *)
  (get_notifications `Waq ~token:waq_token >|= function
   | [
       {
         typ = "reblog";
         account = { id = account_id; _ };
         status = Some { id = status_id; _ };
         _;
       };
     ] ->
       assert (account_id = mstdn1_id);
       assert (status_id = post_id)
   | _ -> assert false);%lwt

  Lwt.return_unit
