let deliver env ?user_id message =
  Db.(e @@ WebPushSubscription.(get_many ~user_id))
  |> List.map (fun s () ->
         match
           Webpush.construct_request ~message ~auth_key:s#key_auth
             ~p256dh_key:s#key_p256dh
             ~subscriber:(Config.webpush_subscriber ())
             ~endpoint:s#endpoint
             ~vapid_priv_key:(Config.vapid_private_key ())
         with
         | Error _ ->
             Logs.err (fun m ->
                 m "Couldn't construct request of webpush: %s" s#endpoint)
         | Ok (headers, body) -> (
             let body = Cstruct.to_string body in
             match
               Throttle_fetch.f env ~headers ~meth:`POST ~body s#endpoint
             with
             | Ok (status, _, _) when Yume.Status.is_success status -> ()
             | Ok (`Gone, _, _) ->
                 Logs.debug (fun m -> m "Subscription gone: %s" s#endpoint);
                 Db.(e WebPushSubscription.(delete [ s ]))
             | _ -> Logs.err (fun m -> m "Couldn't post webpush: %s" s#endpoint)
             ))
  |> Eio.Fiber.all
