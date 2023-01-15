open Common
open Util [@@warning "-33"]

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
          let%lwt body = Controller.Well_known_host_meta.get () in
          respond ~headers:[ ("Content-Type", app_xrd_xml) ] body);
      get "/.well-known/webfinger" (fun req ->
          match query_opt "resource" req with
          | Some [ s ] -> dispatch @@ Controller.Well_known_webfinger.get s
          | _ -> respond ~status:`Not_found "");
      get "/users/:name" (fun req ->
          let username = param ":name" req in
          dispatch @@ Controller.Users.get username);
      post "/users/:name/inbox" (fun req ->
          let%lwt body = body req in
          Log.debug (fun m ->
              m "Inbox:\n%s\n\n%s"
                (headers req
                |> List.map (fun (k, v) -> k ^ ": " ^ v)
                |> String.concat "\n")
                body);
          Controller.Inbox.post body;%lwt
          respond ~status:`Accepted "");
    ]
  in
  let routes_from_clients =
    (* From clients *)
    let dispatch = dispatch app_json in
    let self_id = 1 (* FIXME *) in
    let parse_req req =
      let%lwt body = body req in
      let hs =
        headers req |> List.map (fun (k, v) -> (String.lowercase_ascii k, v))
      in
      Lwt.return
      @@
      match List.assoc_opt "content-type" hs with
      | Some "application/json" -> `Json (Yojson.Safe.from_string body)
      | _ -> `Form (req, body)
    in
    let query name = function
      | `Json (`Assoc l) -> (
          match List.assoc name l with
          | `Bool b -> string_of_bool b
          | `Int i -> string_of_int i
          | `String s -> s
          | _ -> failwith "invalid value")
      | `Json _ -> raise Not_found
      | `Form (req, _body) ->
          (* FIXME: use body *)
          query name req |> List.hd
    in
    let query_opt name req = try Some (query name req) with _ -> None in
    [
      post "/api/v1/accounts/:id/follow" (fun req ->
          let id = param ":id" req |> int_of_string in
          dispatch @@ Controller.Api_v1_accounts_follow.post self_id id);
      post "/api/v1/accounts/:id/unfollow" (fun req ->
          let id = param ":id" req |> int_of_string in
          dispatch @@ Controller.Api_v1_accounts_unfollow.post self_id id);
      get "/api/v1/accounts/search" (fun req ->
          let%lwt req = parse_req req in
          let q = req |> query "q" in
          let resolve =
            req |> query_opt "resolve"
            |> Option.fold ~none:false ~some:bool_of_string
          in
          let re = Regex.compile {|^@?([^@]+)(?:@([^@]+))?$|} in
          match Regex.match_group re q with
          | Ok [ _; username; domain ] ->
              let domain = if domain = "" then None else Some domain in
              dispatch
              @@ Controller.Api_v1_accounts_search.get resolve ~username ~domain
          | _ -> respond ~status:`Bad_request "");
      post "/api/v1/statuses" (fun req ->
          let%lwt req = parse_req req in
          match query_opt "status" req with
          | None -> respond ~status:`Bad_request ""
          | Some status ->
              dispatch @@ Controller.Api_v1_statuses.post self_id status);
    ]
  in
  router (routes_from_servers @ routes_from_clients)
