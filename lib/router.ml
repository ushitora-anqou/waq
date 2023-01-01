let routes =
  let open Http in
  let app_xrd_xml = "application/xrd+xml; charset=utf-8" in
  let app_json = "application/json; charset=utf-8" in
  let app_jrd_json = "application/jrd+json; charset=utf-8" in
  let dispatch content_type t =
    let headers = [ ("Content-Type", content_type) ] in
    match%lwt t with
    | Ok body -> respond ~status:`OK ~headers body
    | Error status -> respond ~status ""
  in
  let routes_from_servers =
    (* From servers *)
    let dispatch = dispatch app_jrd_json in
    [
      get "/.well-known/host-meta" (fun _req ->
          let%lwt body = Job.FromServer.get_well_known_host_meta () in
          respond ~headers:[ ("Content-Type", app_xrd_xml) ] body);
      get "/.well-known/webfinger" (fun req ->
          match query_opt "resource" req with
          | Some [ s ] -> dispatch @@ Job.FromServer.get_well_known_webfinger s
          | _ -> respond ~status:`Not_found "");
      get "/users/:name" (fun req ->
          let username = param ":name" req in
          dispatch @@ Job.FromServer.get_users username);
      post "/users/:name/inbox" (fun req ->
          let%lwt body = body req in
          Log.debug (fun m ->
              m "Inbox:\n%s\n\n%s"
                (headers req
                |> List.map (fun (k, v) -> k ^ ": " ^ v)
                |> String.concat "\n")
                body);
          dispatch @@ Job.FromServer.post_users_inbox body);
    ]
  in
  let routes_from_clients =
    (* From clients *)
    let dispatch = dispatch app_json in
    [
      post "/api/v1/accounts/:id/follow" (fun req ->
          let self_id = 1 (* FIXME *) in
          let id = param ":id" req |> int_of_string in
          dispatch @@ Job.FromClient.post_api_v1_accounts_follow self_id id);
    ]
  in
  router (routes_from_servers @ routes_from_clients)
