open Common

let f =
  make_waq_and_mstdn_scenario @@ fun env waq_token _mstdn_token ->
  (* Lookup @user1 *)
  (match search env `Waq "@user1" with
  | [ a ], _, _ ->
      assert (a.acct = "user1");
      ()
  | _ -> assert false);

  (* Lookup @mstdn1@mstdn_server_domain without token, which should fail *)
  (match search env `Waq ("@mstdn1@" ^ mstdn_server_domain) with
  | [], _, _ -> ()
  | _ -> assert false);

  (* With token, it will succeed *)
  (match
     search env `Waq ~token:waq_token ("@mstdn1@" ^ mstdn_server_domain)
   with
  | [ _ ], _, _ -> ()
  | _ -> assert false);

  ()
