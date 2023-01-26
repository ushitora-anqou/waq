open Activity

(* Recv GET /.well-known/webfinger *)
let get req =
  try%lwt
    let s = req |> Httpx.query "resource" in
    let s =
      (* Remove 'acct:' prefix if exists *)
      if String.starts_with ~prefix:"acct:" s then
        String.sub s 5 (String.length s - 5)
      else s
    in
    (* Get account name and domain and check if they are correct *)
    let s = String.split_on_char '@' s in
    if not (List.length s = 2 && Config.is_my_domain (List.nth s 1)) then
      failwith "Invalid request";
    (* Return the body *)
    let name, dom = (List.hd s, List.nth s 1) in
    let%lwt a =
      Db.Account.get_one_by_domain_and_username ~domain:None ~username:name
    in
    let%lwt _ = Db.User.get_one ~account_id:a.id () in
    make_webfinger
      ~subject:("acct:" ^ name ^ "@" ^ dom)
      ~aliases:[ a.uri ]
      ~links:
        [
          make_webfinger_link ~rel:"self" ~typ:"application/activity+json"
            ~href:a.uri
          |> webfinger_link_to_yojson;
        ]
      ()
    |> webfinger_to_yojson |> Yojson.Safe.to_string
    |> Http.respond ~headers:[ Helper.content_type_app_jrd_json ]
  with e ->
    Log.debug (fun m ->
        m "[well_known_webfinger] Can't find user: %s\n%s"
          (Printexc.to_string e)
          (Printexc.get_backtrace ()));
    Http.raise_error_response `Not_found
