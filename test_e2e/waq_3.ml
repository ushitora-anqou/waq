open Common

let f =
  make_waq_scenario @@ fun waq_token ->
  let%lwt waq_token' = fetch_access_token ~username:"user2" in
  let%lwt user1_id, _, _ = lookup `Waq ~token:waq_token ~username:"user1" () in
  let%lwt user2_id, _, _ = lookup `Waq ~token:waq_token ~username:"user2" () in

  (* Follow @user2 *)
  follow `Waq ~token:waq_token user2_id;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by @user2 *)
  let%lwt { uri; id; _ } = post `Waq ~token:waq_token' () in

  (* check accounts *)
  let%lwt a = get_account `Waq user2_id in
  assert (a.statuses_count = 1);

  (* Reply by me *)
  let%lwt { uri = uri2; id = id2; _ } =
    post `Waq ~token:waq_token ~in_reply_to_id:id ()
  in

  (* check accounts *)
  let%lwt a = get_account `Waq user1_id in
  assert (a.statuses_count = 1);

  (* Reply again *)
  let%lwt { uri = uri3; _ } =
    post `Waq ~token:waq_token ~in_reply_to_id:id2 ()
  in

  (* Get my home timeline and check *)
  (home_timeline `Waq ~token:waq_token >|= function
   | [ `Assoc l3; `Assoc l2; `Assoc l ] ->
       (* Check if the timeline is correct *)
       assert (uri = (l |> List.assoc "uri" |> expect_string));
       assert (id = (l2 |> List.assoc "in_reply_to_id" |> expect_string));
       assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
       assert (id2 = (l3 |> List.assoc "in_reply_to_id" |> expect_string));
       assert (uri3 = (l3 |> List.assoc "uri" |> expect_string));
       ()
   | _ -> assert false);%lwt

  (* Unfollow @user2 *)
  unfollow `Waq ~token:waq_token user2_id;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Get my home timeline and check again *)
  (home_timeline `Waq ~token:waq_token >|= function
   | [ `Assoc l3; `Assoc l2 ] ->
       (* Check if the timeline is correct *)
       assert (uri2 = (l2 |> List.assoc "uri" |> expect_string));
       assert (uri3 = (l3 |> List.assoc "uri" |> expect_string));
       ()
   | _ -> assert false);%lwt

  (* Check status itself *)
  let%lwt s = get_status `Waq id in
  assert (s.uri = uri);

  (* Check the status's context *)
  let%lwt ancestors, descendants = get_status_context `Waq id2 in
  assert (ancestors |> List.map (fun r -> r.uri) = [ uri ]);
  assert (descendants |> List.map (fun r -> r.uri) = [ uri3 ]);

  Lwt.return_unit
