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

  (* Post and reblog by @admin@localhost:3000 *)
  let%lwt { id; uri; _ } = post `Mstdn ~token:mstdn_token () in
  Lwt_unix.sleep 1.0;%lwt
  let%lwt _ = reblog `Mstdn ~token:mstdn_token ~id in
  Lwt_unix.sleep 1.0;%lwt

  (* Get my home timeline and check *)
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

  Lwt.return_unit
