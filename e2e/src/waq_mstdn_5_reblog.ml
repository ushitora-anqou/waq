open Common

let f =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  (* Lookup @admin@localhost:3000 *)
  let%lwt admin_id, _username, _acct =
    lookup `Waq ~token:waq_token ~username:"admin" ~domain:"localhost:3000" ()
  in

  (* Follow @admin@localhost:3000 *)
  follow `Waq ~token:waq_token admin_id;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by user1 *)
  let%lwt { uri; id = post_id; _ } = post `Waq ~token:waq_token () in
  Lwt_unix.sleep 1.0;%lwt

  (* Reblog the post by @admin@locahost:3000 *)
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
       assert (account_id = admin_id);
       assert (status_id = post_id)
   | _ -> assert false);%lwt

  Lwt.return_unit
