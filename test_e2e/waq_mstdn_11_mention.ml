open Common2

let f (a0 : agent) (a1 : agent) =
  let ws_notif = ref None in
  let _, handler =
    websocket_handler_state_machine ~init:`Recv
      ~states:
        [
          ( `Recv,
            fun l _ ->
              assert (List.assoc "stream" l = `List [ `String "user" ]);
              (match List.assoc "event" l |> expect_string with
              | "notification" ->
                  let payload =
                    List.assoc "payload" l |> expect_string
                    |> Yojson.Safe.from_string |> notification_of_yojson
                  in
                  ws_notif := Some payload
              | _ -> ());
              Lwt.return `Recv );
        ]
      ()
  in

  let rest_notif = ref None in
  websocket a1 handler (fun pushf ->
      (* a0: Post with mentions *)
      let%lwt { uri; _ } =
        post a0 ~content:(Printf.sprintf "@%s てすと" (acct_of_agent a1)) ()
      in
      Lwt_unix.sleep 1.0;%lwt

      (* a1: Check its home timeline *)
      let%lwt [ { uri = uri'; _ } ] = home_timeline a1 in
      assert (uri = uri');

      (* a1: Check its notification *)
      let%lwt [ n ] = get_notifications a1 in
      assert ((Option.get n.status).uri = uri);
      assert (n.account.acct = acct_of_agent a0);
      rest_notif := Some n;

      pushf None);%lwt

  assert (Option.is_some !ws_notif);
  assert (Option.is_some !rest_notif);
  assert (!ws_notif = !rest_notif);

  Lwt.return_unit
  [@@warning "-8"]

let f_waq_mstdn =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  let a0 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  let a1 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"admin"
      ~domain:"localhost:3000"
  in
  f a0 a1;%lwt
  Lwt.return_unit

let f_mstdn_waq =
  make_waq_and_mstdn_scenario @@ fun waq_token mstdn_token ->
  let a0 =
    make_agent ~kind:`Mstdn ~token:mstdn_token ~username:"admin"
      ~domain:"localhost:3000"
  in
  let a1 =
    make_agent ~kind:`Waq ~token:waq_token ~username:"user1"
      ~domain:waq_server_domain
  in
  f a0 a1;%lwt
  Lwt.return_unit
