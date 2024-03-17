open Common

let f =
  make_waq_and_mstdn_scenario @@ fun waq_token _mstdn_token ->
  (* Lookup @user1 *)
  (match%lwt search `Waq "@user1" with
  | [ a ], _, _ ->
      assert (a.acct = "user1");
      Lwt.return_unit
  | _ -> assert false);%lwt

  (* Lookup @mstdn1@mstdn_server_domain without token, which should fail *)
  (match%lwt search `Waq ("@mstdn1@" ^ mstdn_server_domain) with
  | [], _, _ -> Lwt.return_unit
  | _ -> assert false);%lwt

  (* With token, it will succeed *)
  (match%lwt
     search `Waq ~token:waq_token ("@mstdn1@" ^ mstdn_server_domain)
   with
  | [ _ ], _, _ -> Lwt.return_unit
  | _ -> assert false);%lwt

  Lwt.return_unit
