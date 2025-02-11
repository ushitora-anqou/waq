module Ptime = struct
  include Ptime

  let now () = Unix.gettimeofday () |> of_float_s |> Option.get
  let to_int x = x |> to_span |> Span.to_int_s |> Option.get
  let to_rfc3339 = to_rfc3339 ~tz_offset_s:0

  let to_http_date (v : t) : string =
    let string_of_week = function
      | `Sun -> "Sun"
      | `Mon -> "Mon"
      | `Tue -> "Tue"
      | `Wed -> "Wed"
      | `Thu -> "Thu"
      | `Fri -> "Fri"
      | `Sat -> "Sat"
    in
    let string_of_month = function
      | 1 -> "Jan"
      | 2 -> "Feb"
      | 3 -> "Mar"
      | 4 -> "Apr"
      | 5 -> "May"
      | 6 -> "Jun"
      | 7 -> "Jul"
      | 8 -> "Aug"
      | 9 -> "Sep"
      | 10 -> "Oct"
      | 11 -> "Nov"
      | 12 -> "Dec"
      | _ -> assert false
    in
    let (year, month, day_of_month), ((hour, minute, second), _) =
      to_date_time v
    in
    let month = string_of_month month in
    let day_name = weekday v |> string_of_week in
    Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d GMT" day_name day_of_month
      month year hour minute second
end

module Uri = struct
  include Uri

  let getaddrinfo_port (u : t) =
    let scheme = Uri.scheme u |> Option.get in
    u |> Uri.port |> Option.fold ~none:scheme ~some:string_of_int

  let http_host (u : t) =
    let host = Uri.host u |> Option.get in
    match Uri.port u with
    | None -> host
    | Some port -> host ^ ":" ^ string_of_int port

  let path_query_fragment (u : t) =
    let res = Uri.path u in
    let res =
      match Uri.verbatim_query u with None -> res | Some q -> res ^ "?" ^ q
    in
    let res =
      match Uri.fragment u with None -> res | Some f -> res ^ "#" ^ f
    in
    res

  let domain (u : t) = http_host u
end

module Internal = struct
  let kubectl_path = Sys.getenv "KUBECTL"
  let manifests = Sys.getenv "MANIFESTS" ^ "/"

  let kubectl args f =
    Logs.info (fun m ->
        m "execute: %s" (Filename.quote_command kubectl_path args));
    let open Unix in
    let ic =
      open_process_args_in kubectl_path (Array.of_list (kubectl_path :: args))
    in
    try
      let res = f ic in
      let _ = In_channel.input_all ic (* necessary to avoid SIGPIPE *) in
      (close_process_in ic, res)
    with e ->
      close_process_in ic |> ignore;
      raise e

  let port_forward ~ns ~svc ~ports f =
    kubectl
      [ "port-forward"; "-n"; ns; "--address"; "0.0.0.0"; "svc/" ^ svc; ports ]
      (fun ic ->
        let pid = Unix.process_in_pid ic in
        Fun.protect ~finally:(fun () -> Unix.kill pid Sys.sigint) f)
    |> ignore

  let wait ?(num_retries = 5) ?(timeout = "5m") ~for_condition target =
    let rec loop i =
      if i >= num_retries then assert false
      else
        match
          kubectl
            [
              "wait";
              "--for=condition=" ^ for_condition;
              target;
              "-n";
              "e2e";
              "--timeout=" ^ timeout;
            ]
            ignore
          |> fst
        with
        | Unix.WEXITED 0 -> ()
        | _ ->
            Unix.sleep 10;
            loop (i + 1)
    in
    loop 0

  let expect_wexited_0 = function
    | Unix.WEXITED 0 -> ()
    | WEXITED i ->
        Logs.err (fun m -> m "expect WEXITED 0 but got WEXITED %d" i);
        assert false
    | WSIGNALED i ->
        Logs.err (fun m -> m "expect WEXITED 0 but got WSIGNALED %d" i);
        assert false
    | WSTOPPED i ->
        Logs.err (fun m -> m "expect WEXITED 0 but got WSTOPPED %d" i);
        assert false

  let launch_waq f =
    let () =
      kubectl
        [ "scale"; "-n"; "e2e"; "deploy"; "waq-web"; "--replicas=0" ]
        ignore
      |> fst |> expect_wexited_0
    in
    let _ =
      kubectl [ "delete"; "job"; "reset-waq-database"; "-n"; "e2e" ] ignore
    in
    let () =
      kubectl [ "apply"; "-f"; manifests ^ "reset-waq-database.yaml" ] ignore
      |> fst |> expect_wexited_0
    in
    let () = wait ~for_condition:"complete" "job/reset-waq-database" in
    let () =
      kubectl
        [ "scale"; "-n"; "e2e"; "deploy"; "waq-web"; "--replicas=1" ]
        ignore
      |> fst |> expect_wexited_0
    in

    let generate_token username =
      let rec loop i =
        if i > 10 then
          failwith "timeout: couldn't get access tokens for Mastodon";
        Unix.sleep 5;
        let token =
          kubectl
            [
              "exec";
              "-n";
              "e2e";
              "deploy/waq-web";
              "--";
              "sh";
              "-c";
              "waq oauth:generate_access_token " ^ username ^ " 2> /dev/null";
            ]
            (fun ic -> In_channel.input_line ic |> Option.value ~default:"")
          |> snd
        in
        if token = "" then loop (i + 1) else token
      in
      loop 0
    in
    let token1 = generate_token "user1" in
    let token2 = generate_token "user2" in
    let token3 = generate_token "user3" in

    port_forward ~ns:"e2e" ~svc:"waq-web" ~ports:"58080:8000" @@ fun () ->
    f [| token1; token2; token3 |];
    ()

  let launch_mastodon f =
    let () = wait ~for_condition:"available" "deploy/mastodon-web" in

    let () =
      kubectl
        [ "scale"; "-n"; "e2e"; "deploy"; "mastodon-web"; "--replicas=0" ]
        ignore
      |> fst |> expect_wexited_0
    in
    let () =
      kubectl
        [ "scale"; "-n"; "e2e"; "deploy"; "mastodon-streaming"; "--replicas=0" ]
        ignore
      |> fst |> expect_wexited_0
    in
    let () =
      kubectl
        [ "scale"; "-n"; "e2e"; "deploy"; "mastodon-sidekiq"; "--replicas=0" ]
        ignore
      |> fst |> expect_wexited_0
    in
    let _, () =
      kubectl
        [ "delete"; "-f"; manifests ^ "reset-mastodon-database.yaml" ]
        ignore
    in
    let () =
      kubectl
        [ "apply"; "-f"; manifests ^ "reset-mastodon-database.yaml" ]
        ignore
      |> fst |> expect_wexited_0
    in
    let () = wait ~for_condition:"complete" "job/reset-mastodon-database" in
    let () =
      kubectl
        [ "scale"; "-n"; "e2e"; "deploy"; "mastodon-web"; "--replicas=1" ]
        ignore
      |> fst |> expect_wexited_0
    in
    let () =
      kubectl
        [ "scale"; "-n"; "e2e"; "deploy"; "mastodon-streaming"; "--replicas=1" ]
        ignore
      |> fst |> expect_wexited_0
    in
    let () =
      kubectl
        [ "scale"; "-n"; "e2e"; "deploy"; "mastodon-sidekiq"; "--replicas=1" ]
        ignore
      |> fst |> expect_wexited_0
    in
    let () =
      kubectl
        [
          "rollout"; "restart"; "-n"; "e2e"; "deploy"; "mastodon-gateway-nginx";
        ]
        ignore
      |> fst |> expect_wexited_0
    in

    Logs.info (fun m -> m "Resetting database for Mastodon");
    let token1, token2, token3 =
      let rec loop i =
        if i > 10 then
          failwith "timeout: couldn't get access tokens for Mastodon";
        Unix.sleep 5;
        match
          kubectl [ "logs"; "-n"; "e2e"; "job/reset-mastodon-database" ]
            (fun ic ->
              List.init 3 (fun _ ->
                  In_channel.input_line ic |> Option.value ~default:""))
          |> snd
        with
        | [ token1; token2; token3 ] -> (token1, token2, token3)
        | _ -> loop (i + 1)
      in
      loop 0
    in

    port_forward ~ns:"e2e" ~svc:"mastodon-gateway" ~ports:"58081:80"
    @@ fun () ->
    f [| token1; token2; token3 |];
    ()
end

let ( ^/ ) a b = a ^ "/" ^ b
let fetch = Yume.Client.fetch
let fetch_exn = Yume.Client.fetch_exn

let expect_string = function
  | `String s -> s
  | _ -> failwith "Expected string, got something different"

let expect_assoc = function
  | `Assoc l -> l
  | _ -> failwith "Expected assoc, got something different"

let expect_list = function
  | `List l -> l
  | _ -> failwith "Expected list, got something different"

let expect_bool = function
  | `Bool b -> b
  | _ -> failwith "Expected bool, got something different"

let expect_int = function
  | `Int v -> v
  | _ -> failwith "Expected int, got something different"

let with_lock mtx f =
  match mtx with
  | None -> f ()
  | Some mtx -> Eio.Mutex.use_rw ~protect:true mtx f

let new_session f =
  Internal.launch_waq @@ fun tokens ->
  f tokens.(0);
  ()

let new_mastodon_session f =
  Internal.launch_mastodon @@ fun tokens ->
  f tokens.(0);
  ()

let make_waq_and_mstdn_scenario ?(timeout = 30.0) handler () : unit =
  new_session @@ fun waq_token ->
  Logs.debug (fun m -> m "Access token for Waq: %s" waq_token);
  new_mastodon_session @@ fun mstdn_token ->
  Logs.debug (fun m -> m "Access token for Mastodon: %s" mstdn_token);
  Unix.sleep 10;
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Time.with_timeout_exn env#clock timeout (fun () ->
      handler env waq_token mstdn_token)

let make_waq_scenario ?(timeout = 30.0) handler () : unit =
  new_session @@ fun waq_token ->
  Logs.debug (fun m -> m "Access token for Waq: %s" waq_token);
  Unix.sleep 1;
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Time.with_timeout_exn env#clock timeout (fun () -> handler env waq_token)

let make_mstdn_scenario ?(timeout = 30.0) handler () : unit =
  new_mastodon_session @@ fun mstdn_token ->
  Logs.debug (fun m -> m "Access token for Mastodon: %s" mstdn_token);
  Unix.sleep 10;
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Time.with_timeout_exn env#clock timeout (fun () ->
      handler env mstdn_token)

let waq_server_name = Sys.getenv "E2E_TEST_WAQ_SERVER_NAME"
let waq_server_domain = Uri.(of_string waq_server_name |> domain)
let waq url = waq_server_name ^ url
let mstdn_server_name = Sys.getenv "E2E_TEST_MASTODON_SERVER_NAME"
let mstdn_server_domain = Uri.(of_string mstdn_server_name |> domain)
let mstdn url = mstdn_server_name ^ url
let url = function `Waq -> waq | `Mstdn -> mstdn

let pp_json (s : string) =
  Logs.debug (fun m -> m "%s" Yojson.Safe.(from_string s |> pretty_to_string))
[@@warning "-32"]

let do_fetch env ?token ?(meth = `GET) ?(body = "") kind target =
  let headers = [ (`Accept, "application/json") ] in
  let headers =
    match meth with
    | `POST -> (`Content_type, "application/json") :: headers
    | _ -> headers
  in
  let headers =
    match token with
    | Some token -> (`Authorization, "Bearer " ^ token) :: headers
    | None -> headers
  in
  fetch_exn env ~headers ~meth ~body (url kind target)

type account = {
  id : string;
  username : string;
  acct : string;
  display_name : string;
  note : string;
  last_status_at : string option;
  statuses_count : int;
  followers_count : int;
  following_count : int;
  avatar : string;
  header : string;
  bot : bool;
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

type media_attachment = {
  id : string;
  type_ : string; [@key "type"]
  url : string;
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

type preview_card = { url : string }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type status_mention = {
  id : string;
  username : string;
  url : string;
  acct : string;
}
[@@deriving make, yojson]

type status = {
  id : string;
  uri : string;
  reblog : status option;
  reblogged : bool;
  reblogs_count : int;
  favourited : bool;
  account : account;
  favourites_count : int;
  media_attachments : media_attachment list;
  spoiler_text : string;
  mentions : status_mention list;
  card : preview_card option;
  content : string option; [@yojson.option]
  muted : bool option; [@yojson.option]
  bookmarked : bool option; [@yojson.option]
  pinned : bool option; [@yojson.option]
  filtered : string (* FIXME: dummy *) list option; [@yojson.option]
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

type relationship = { id : string; following : bool; followed_by : bool }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type notification = {
  id : string;
  typ : string; [@key "type"]
  created_at : string;
  account : account;
  status : status option; [@yojson.option]
}
[@@deriving make, yojson]

type marker = { last_read_id : string; version : int; updated_at : string }
[@@deriving make, yojson]

let update_credentials env ~token kind ?display_name ?note ?avatar ?header ?bot
    () =
  let target = "/api/v1/accounts/update_credentials" in
  let headers =
    [
      (`Accept, "application/json");
      (`Authorization, "Bearer " ^ token);
      ( `Content_type,
        "multipart/form-data; \
         boundary=---------------------------91791948726096252761377705945" );
    ]
  in
  let body =
    [ {|-----------------------------91791948726096252761377705945--|}; {||} ]
  in
  let body =
    match bot with
    | None -> body
    | Some bot ->
        [
          {|-----------------------------91791948726096252761377705945|};
          {|Content-Disposition: form-data; name="bot"|};
          {||};
          string_of_bool bot;
        ]
        @ body
  in
  let body =
    match note with
    | None -> body
    | Some note ->
        [
          {|-----------------------------91791948726096252761377705945|};
          {|Content-Disposition: form-data; name="note"|};
          {||};
          note;
        ]
        @ body
  in
  let body =
    match display_name with
    | None -> body
    | Some display_name ->
        [
          {|-----------------------------91791948726096252761377705945|};
          {|Content-Disposition: form-data; name="display_name"|};
          {||};
          display_name;
        ]
        @ body
  in
  let body =
    match avatar with
    | None -> body
    | Some avatar ->
        [
          {|-----------------------------91791948726096252761377705945|};
          {|Content-Disposition: form-data; name="avatar"; filename="avatar.png"|};
          {|Content-Type: image/png|};
          {||};
          avatar;
        ]
        @ body
  in
  let body =
    match header with
    | None -> body
    | Some header ->
        [
          {|-----------------------------91791948726096252761377705945|};
          {|Content-Disposition: form-data; name="header"; filename="header.png"|};
          {|Content-Type: image/png|};
          {||};
          header;
        ]
        @ body
  in
  assert (List.length body <> 2);
  let body = String.concat "\r\n" body in
  fetch_exn env ~headers ~meth:`PATCH ~body (url kind target)
  |> Yojson.Safe.from_string |> account_of_yojson

let lookup_via_v1_accounts_lookup env ~token kind ?domain ~username () =
  let target =
    let src = "/api/v1/accounts/lookup?acct=" in
    match domain with
    | None -> src ^ username
    | Some domain -> src ^ username ^ "@" ^ domain
  in
  let r = do_fetch env ~token kind target in
  let a = r |> Yojson.Safe.from_string |> account_of_yojson in
  (a.id, a.username, a.acct)

let lookup_via_v1_accounts_search env ?token kind ?domain ~username () =
  let target =
    let src = "/api/v1/accounts/search?resolve=true&q=@" in
    match domain with
    | None -> src ^ username
    | Some domain -> src ^ username ^ "@" ^ domain
  in
  let r = do_fetch env ?token kind target in
  let l =
    match Yojson.Safe.from_string r with
    | `List [ `Assoc l ] -> l
    | _ -> assert false
  in
  ( l |> List.assoc "id" |> expect_string,
    l |> List.assoc "username" |> expect_string,
    l |> List.assoc "acct" |> expect_string )

let search env ?token kind q =
  let queries = [ ("resolve", [ "true" ]); ("q", [ q ]) ] in
  let u = Uri.of_string "/api/v2/search" in
  let u = Uri.add_query_params u queries in
  let r = do_fetch env ?token kind (Uri.to_string u) in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  ( List.assoc "accounts" l |> expect_list |> List.map account_of_yojson,
    List.assoc "statuses" l |> expect_list |> List.map status_of_yojson,
    List.assoc "hashtags" l |> expect_list )

let lookup env ~token kind ?domain ~username () =
  match
    search env ~token kind
      (domain
      |> Option.fold ~none:("@" ^ username) ~some:(fun domain ->
             "@" ^ username ^ "@" ^ domain))
  with
  | [ acct ], _, _ -> (acct.id, acct.username, acct.acct)
  | accts, _, _ ->
      Logs.err (fun m ->
          m "lookup failed: accts=[%s]"
            (accts
            |> List.map (fun (a : account) -> a.acct)
            |> String.concat ", "));
      assert false

let get_account env kind id =
  do_fetch env kind ("/api/v1/accounts/" ^ id)
  |> Yojson.Safe.from_string |> account_of_yojson

let get_relationships env ~token kind account_ids =
  let target =
    "/api/v1/accounts/relationships?"
    ^ (account_ids |> List.map (fun id -> "id[]=" ^ id) |> String.concat "&")
  in
  do_fetch env ~token kind target
  |> Yojson.Safe.from_string |> expect_list
  |> List.map relationship_of_yojson

let get_followers env ?token kind account_id =
  do_fetch env ?token kind ("/api/v1/accounts/" ^ account_id ^ "/followers")
  |> Yojson.Safe.from_string |> expect_list |> List.map account_of_yojson

let get_following env ?token kind account_id =
  do_fetch env ?token kind ("/api/v1/accounts/" ^ account_id ^ "/following")
  |> Yojson.Safe.from_string |> expect_list |> List.map account_of_yojson

let get_notifications env ?token kind =
  do_fetch env ?token kind "/api/v1/notifications"
  |> Yojson.Safe.from_string |> expect_list
  |> List.map notification_of_yojson

let markers_of_yojson j =
  let l = expect_assoc j in
  ( List.assoc_opt "home" l |> Option.map marker_of_yojson,
    List.assoc_opt "notifications" l |> Option.map marker_of_yojson )

let get_markers env ?token kind timelines =
  do_fetch env ?token kind
    ("/api/v1/markers?"
    ^ (timelines |> List.map (fun s -> "timeline[]=" ^ s) |> String.concat "&")
    )
  |> Yojson.Safe.from_string |> markers_of_yojson

let post_markers env ?token kind values =
  let body =
    values
    |> List.map (fun (timeline, last_read_id) ->
           (timeline, `Assoc [ ("last_read_id", `String last_read_id) ]))
    |> fun l -> `Assoc l |> Yojson.Safe.to_string
  in
  do_fetch env ~meth:`POST ?token kind ~body "/api/v1/markers"
  |> Yojson.Safe.from_string |> markers_of_yojson

let follow env ~token kind account_id =
  let r =
    do_fetch env ~meth:`POST ~token kind
      ("/api/v1/accounts/" ^ account_id ^ "/follow")
  in
  assert (
    Yojson.Safe.from_string r |> expect_assoc |> List.assoc "following"
    |> expect_bool);
  ()

let unfollow env ~token kind account_id =
  do_fetch env ~meth:`POST ~token kind
    ("/api/v1/accounts/" ^ account_id ^ "/unfollow")
  |> ignore

let get_status env kind ?token status_id =
  do_fetch env ?token kind ("/api/v1/statuses/" ^ status_id)
  |> Yojson.Safe.from_string |> status_of_yojson

let get_account_statuses env kind ?token ?(exclude_replies = false)
    ?(pinned = false) account_id =
  do_fetch env ?token kind
    ("/api/v1/accounts/" ^ account_id ^ "/statuses?exclude_replies="
    ^ string_of_bool exclude_replies
    ^ "&pinned=" ^ string_of_bool pinned)
  |> Yojson.Safe.from_string |> expect_list |> List.map status_of_yojson

let get_status_context env kind status_id =
  let r = do_fetch env kind ("/api/v1/statuses/" ^ status_id ^ "/context") in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  match l with
  | [ ("ancestors", `List ancestors); ("descendants", `List descendants) ]
  | [ ("descendants", `List descendants); ("ancestors", `List ancestors) ] ->
      let ancestors = ancestors |> List.map status_of_yojson in
      let descendants = descendants |> List.map status_of_yojson in
      (ancestors, descendants)
  | _ -> assert false

let post env ~token kind ?spoiler_text ?content ?in_reply_to_id
    ?(media_ids = []) () =
  let content = content |> Option.value ~default:"こんにちは、世界！" in
  let body =
    let l =
      [
        ("status", `String content);
        ("media_ids", `List (media_ids |> List.map (fun s -> `String s)));
      ]
    in
    let l =
      in_reply_to_id
      |> Option.fold ~none:l ~some:(fun id ->
             ("in_reply_to_id", `String id) :: l)
    in
    let l =
      spoiler_text
      |> Option.fold ~none:l ~some:(fun s -> ("spoiler_text", `String s) :: l)
    in
    `Assoc l |> Yojson.Safe.to_string
  in
  do_fetch env ~token ~meth:`POST ~body kind "/api/v1/statuses"
  |> Yojson.Safe.from_string |> status_of_yojson

let delete_status env ~token kind status_id =
  do_fetch env ~token ~meth:`DELETE kind ("/api/v1/statuses/" ^ status_id)
  |> Yojson.Safe.from_string |> status_of_yojson

let reblog env ~token kind ~id =
  do_fetch env ~token ~meth:`POST kind ("/api/v1/statuses/" ^ id ^ "/reblog")
  |> Yojson.Safe.from_string |> status_of_yojson

let unreblog env ~token kind ~id =
  do_fetch env ~token ~meth:`POST kind ("/api/v1/statuses/" ^ id ^ "/unreblog")
  |> Yojson.Safe.from_string |> status_of_yojson

let fav env ~token kind ~id =
  do_fetch env ~token ~meth:`POST kind ("/api/v1/statuses/" ^ id ^ "/favourite")
  |> Yojson.Safe.from_string |> status_of_yojson

let unfav env ~token kind ~id =
  do_fetch env ~token ~meth:`POST kind
    ("/api/v1/statuses/" ^ id ^ "/unfavourite")
  |> Yojson.Safe.from_string |> status_of_yojson

let get_favourited_by env ~token kind ~id =
  do_fetch env ~token ~meth:`GET kind
    ("/api/v1/statuses/" ^ id ^ "/favourited_by")
  |> Yojson.Safe.from_string |> expect_list |> List.map account_of_yojson

let home_timeline env ~token kind =
  do_fetch env ~token kind "/api/v1/timelines/home" |> fun r ->
  match Yojson.Safe.from_string r with `List l -> l | _ -> assert false

let fetch_access_token env ~username =
  let r =
    fetch_exn env ~meth:`POST
      ~headers:[ (`Content_type, "application/json") ]
      ~body:
        {|{"client_name":"foo","redirect_uris":"http://example.com?origin=http://example.com"}|}
      (waq "/api/v1/apps")
  in
  let client_id, client_secret =
    match Yojson.Safe.from_string r with
    | `Assoc l ->
        ( List.assoc "client_id" l |> expect_string,
          List.assoc "client_secret" l |> expect_string )
    | _ -> assert false
  in

  let r =
    let body =
      Uri.encoded_of_query
        [
          ("response_type", [ "code" ]);
          ("client_id", [ client_id ]);
          ("redirect_uri", [ "http://example.com?origin=http://example.com" ]);
          ("username", [ username ]);
          ("password", [ username ^ "password" ]);
        ]
    in
    (* NOTE: The header "content-type: application/x-www-form-urlencoded"
       should be specified explicitly like the following to send the POST
       request body correctly via Tunnelmole. *)
    fetch env
      ~headers:[ (`Content_type, "application/x-www-form-urlencoded") ]
      ~meth:`POST ~body (waq "/oauth/authorize")
  in
  let auth_code =
    match r with
    | Ok (`Found, headers, _body) ->
        (* 0123456789012345678901234
           http://example.com?code=... *)
        headers |> List.assoc `Location |> Uri.of_string |> Uri.query
        |> List.assoc "code" |> List.hd
    | _ -> assert false
  in

  let r =
    fetch_exn env ~meth:`POST
      ~headers:[ (`Content_type, "application/json") ]
      ~body:
        (`Assoc
           [
             ("grant_type", `String "authorization_code");
             ("code", `String auth_code);
             ("client_id", `String client_id);
             ("client_secret", `String client_secret);
             ( "redirect_uri",
               `String "http://example.com?origin=http://example.com" );
           ]
        |> Yojson.Safe.to_string)
      (waq "/oauth/token")
  in
  match Yojson.Safe.from_string r with
  | `Assoc l -> l |> List.assoc "access_token" |> expect_string
  | _ -> assert false

let websocket env ?mtx ~token kind ?target handler f =
  let target =
    match target with
    | Some target -> target
    | None -> "/api/v1/streaming?stream=user"
  in
  let extra_headers =
    Cohttp.Header.of_list [ ("Sec-WebSocket-Protocol", token) ]
  in
  Eio.Switch.run @@ fun sw ->
  let conn = Yume.Ws.Client.connect env ~sw ~extra_headers (url kind target) in
  let close_sent = ref false in
  let pushf msg =
    match msg with
    | Some content ->
        Yume.Ws.Client.write conn (Websocket.Frame.create ~content ())
    | None when !close_sent -> ()
    | None ->
        Yume.Ws.Client.write conn (Websocket.Frame.create ~opcode:Close ());
        close_sent := true
  in
  let rec react () =
    match Yume.Ws.Client.read conn with
    | { Websocket.Frame.opcode = Ping; _ } ->
        Yume.Ws.Client.write conn (Websocket.Frame.create ~opcode:Pong ());
        react ()
    | { opcode = Pong; _ } -> react ()
    | { opcode = Text; content; _ } | { opcode = Binary; content; _ } ->
        with_lock mtx (fun () -> handler content pushf);
        react ()
    | { opcode = Close; content; _ } ->
        if !close_sent then ()
        else if String.length content >= 2 then
          Yume.Ws.Client.write conn
            (Websocket.Frame.create ~opcode:Close
               ~content:(String.sub content 0 2) ())
        else Yume.Ws.Client.write conn (Websocket.Frame.close 1000);
        Yume.Ws.Client.close_transport conn
    | _ -> Yume.Ws.Client.close_transport conn
  in
  Eio.Fiber.both
    (fun () -> react ())
    (fun () -> with_lock mtx (fun () -> f pushf))

let websocket_handler_state_machine ~states ~init () =
  let current = ref init in
  let set_current v = current := v in
  let handler content pushf =
    let real_handler = states |> List.assoc !current in
    let next_state =
      real_handler (content |> Yojson.Safe.from_string |> expect_assoc) pushf
    in
    set_current next_state;
    ()
  in
  (set_current, handler)

let websocket_stack env kind ~token ?num_msgs f =
  let recv_msgs = ref [] in
  let handler content pushf =
    recv_msgs := content :: !recv_msgs;
    match num_msgs with
    | Some num_msgs when List.length !recv_msgs = num_msgs -> pushf None
    | _ -> ()
  in
  websocket env kind ~token handler (fun pushf ->
      f pushf;
      match num_msgs with None -> pushf None | Some _ -> ());
  !recv_msgs

let expect_exc f =
  try
    let _ = f () in
    assert false
  with _ -> ()

let test_image =
  {|
iVBORw0KGgoAAAANSUhEUgAAADIAAAAyAQAAAAA2RLUcAAAABGdBTUEAALGPC/xhBQAAACBjSFJN
AAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAAmJLR0QAAd2KE6QAAAAHdElN
RQfnAxYCJTrYPC4yAAAADklEQVQY02NgGAWDCQAAAZAAAcWb20kAAAAldEVYdGRhdGU6Y3JlYXRl
ADIwMjMtMDMtMjJUMDI6Mzc6NTgrMDA6MDClQ3CPAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTAz
LTIyVDAyOjM3OjU4KzAwOjAw1B7IMwAAAABJRU5ErkJggg==|}
  |> String.trim |> String.split_on_char '\n' |> String.concat ""
  |> Base64.decode_exn

let test_image_large =
  {|
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
AAIBAwADAAAAhgAAABIBAwABAAAAAQAAABoBBQABAAAAjAAAABsBBQABAAAAlAAAACgBAwABAAAA
AgAAADEBAgANAAAAnAAAADIBAgAUAAAAqgAAAGmHBAABAAAAvgAAAAAAAAAIAAgACABIAAAAAQAA
AEgAAAABAAAAR0lNUCAyLjEwLjM0AAAyMDIzOjA1OjE1IDE4OjA5OjE0AAEAAaADAAEAAAABAAAA
AAAAAEf4jE8AAAAldEVYdGRhdGU6Y3JlYXRlADIwMjQtMDYtMDJUMDk6MTM6MzArMDA6MDBp4rJK
AAAAJXRFWHRkYXRlOm1vZGlmeQAyMDI0LTA2LTAyVDA5OjEzOjMwKzAwOjAwGL8K9gAAABp0RVh0
ZXhpZjpCaXRzUGVyU2FtcGxlADgsIDgsIDgS7T4nAAAAEXRFWHRleGlmOkNvbG9yU3BhY2UAMQ+b
AkkAAAAhdEVYdGV4aWY6RGF0ZVRpbWUAMjAyMzowNToxNSAxODowOToxNBJwRVYAAAATdEVYdGV4
aWY6RXhpZk9mZnNldAAxOTBMjvPCAAAAFHRFWHRleGlmOkltYWdlTGVuZ3RoADI0MvW9M3QAAAAT
dEVYdGV4aWY6SW1hZ2VXaWR0aAAyNDImwSP5AAAAGnRFWHRleGlmOlNvZnR3YXJlAEdJTVAgMi4x
MC4zNBhmc5oAAAAbdEVYdGljYzpjb3B5cmlnaHQAUHVibGljIERvbWFpbraRMVsAAAAidEVYdGlj
YzpkZXNjcmlwdGlvbgBHSU1QIGJ1aWx0LWluIHNSR0JMZ0ETAAAAFXRFWHRpY2M6bWFudWZhY3R1
cmVyAEdJTVBMnpDKAAAADnRFWHRpY2M6bW9kZWwAc1JHQltgSUMAAAAASUVORK5CYII=|}
  |> String.trim |> String.split_on_char '\n' |> String.concat ""
  |> Base64.decode_exn
