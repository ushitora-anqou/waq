open Common
open Activity

(* Recv GET /.well-known/webfinger *)
let get s =
  try%lwt
    let s =
      (* Remove 'acct:' prefix if exists *)
      if String.starts_with ~prefix:"acct:" s then
        String.sub s 5 (String.length s - 5)
      else s
    in
    (* Get account name and domain and check if they are correct *)
    let s = String.split_on_char '@' s in
    if not (List.length s = 2 && Config.is_my_domain (List.nth s 1)) then
      raise (failwith "Invalid request");
    (* Return the body *)
    let name, dom = (List.hd s, List.nth s 1) in
    let%lwt _ = Db.User.get ~by:(`username name) in
    let%lwt a = Db.Account.get ~by:(`domain_username (None, name)) in
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
    |> webfinger_to_yojson |> Yojson.Safe.to_string |> Result.ok |> Lwt.return
  with e ->
    Log.debug (fun m ->
        m "[well_known_webfinger] Can't find user: %s\n%s"
          (Printexc.to_string e)
          (Printexc.get_backtrace ()));
    Lwt.return (Error `Not_found)
