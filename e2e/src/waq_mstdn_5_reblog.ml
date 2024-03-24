open Common

let f =
  make_waq_and_mstdn_scenario @@ fun env waq_token mstdn_token ->
  (* Lookup @mstdn1@mstdn_server_domain *)
  let mstdn1_id, _username, _acct =
    lookup env `Waq ~token:waq_token ~username:"mstdn1"
      ~domain:mstdn_server_domain ()
  in

  (* Follow @mstdn1@mstdn_server_domain *)
  follow env `Waq ~token:waq_token mstdn1_id;
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Post by user1 *)
  let ({ uri; id = post_id; _ } : status) = post env `Waq ~token:waq_token () in
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Reblog the post by @mstdn1@locahost:3000 *)
  (match search env `Mstdn ~token:mstdn_token uri with
  | _, [ status ], _ ->
      reblog env `Mstdn ~token:mstdn_token ~id:status.id |> ignore
  | _ -> assert false);
  Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;

  (* Check home timeline *)
  (home_timeline env `Waq ~token:waq_token |> function
   | [ `Assoc l1; `Assoc l2 ] ->
       (* Check if the timeline is correct *)
       assert (uri = (l2 |> List.assoc "uri" |> expect_string));
       assert (
         uri
         = (l1 |> List.assoc "reblog" |> expect_assoc |> List.assoc "uri"
          |> expect_string));
       ()
   | _ -> assert false);

  (* Check notifications *)
  (get_notifications env `Waq ~token:waq_token |> function
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
   | _ -> assert false);

  ()
