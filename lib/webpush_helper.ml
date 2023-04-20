open Lwt.Infix

let deliver ?user_id message =
  Db.(e @@ WebPushSubscription.(get_many ~user_id))
  >>= Lwt_list.iter_p @@ fun s ->
      match
        Webpush.construct_request ~message ~auth_key:s#key_auth
          ~p256dh_key:s#key_p256dh
          ~subscriber:(Config.webpush_subscriber ())
          ~endpoint:s#endpoint
          ~vapid_priv_key:(Config.vapid_private_key ())
      with
      | Error _ ->
          Logq.err (fun m ->
              m "Couldn't construct request of webpush: %s" s#endpoint);
          Lwt.return_unit
      | Ok (headers, body) -> (
          let body = Cstruct.to_string body in
          match%lwt Throttle_fetch.f ~headers ~meth:`POST ~body s#endpoint with
          | Ok (status, _, _) when Httpq.Status.is_success status ->
              Lwt.return_unit
          | Ok (`Gone, _, _) ->
              Logq.debug (fun m -> m "Subscription gone: %s" s#endpoint);
              Db.(e WebPushSubscription.(delete [ s ]))
          | _ ->
              Logq.err (fun m -> m "Couldn't post webpush: %s" s#endpoint);
              Lwt.return_unit)
