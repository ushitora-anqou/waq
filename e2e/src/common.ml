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
    Logq.info (fun m ->
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
        Logq.err (fun m -> m "expect WEXITED 0 but got WEXITED %d" i);
        assert false
    | WSIGNALED i ->
        Logq.err (fun m -> m "expect WEXITED 0 but got WSIGNALED %d" i);
        assert false
    | WSTOPPED i ->
        Logq.err (fun m -> m "expect WEXITED 0 but got WSTOPPED %d" i);
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
            (["exec"; "-n"; "e2e"; "deploy/waq-web"; "--"; "bash"; "-ce";
            "/waq/waq oauth:generate_access_token " ^ username ^ " 2> /dev/null"] [@ocamlformat "disable"])
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

    Logq.info (fun m -> m "Resetting database for Mastodon");
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

let fetch env ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) url =
  let open Yume in
  let uri = Uri.of_string url in

  (* NOTE: Ad-hoc scheme rewriting (https -> http) for localhost
     for better dev experience *)
  let uri =
    match Uri.scheme uri with
    | Some "https"
      when [ Some "localhost"; Some "127.0.0.1" ] |> List.mem (Uri.host uri) ->
        Uri.with_scheme uri (Some "http")
    | _ -> uri
  in

  let meth_s = Method.to_string meth in
  let headers =
    let headers =
      let add (k, v) headers =
        if List.mem_assoc k headers then headers else (k, v) :: headers
      in
      headers
      |> add (`Content_length, body |> String.length |> string_of_int)
      |> add (`Connection, "close")
      |> add (`Host, Uri.http_host uri)
      |> add (`Date, Ptime.(now () |> to_http_date))
    in
    let headers =
      match sign with
      | None -> headers
      | Some (priv_key, key_id, signed_headers) ->
          Signature.sign ~priv_key ~key_id ~signed_headers ~headers ~meth
            ~path:(Uri.path_query_fragment uri)
            ~body:(Some body)
    in
    Headers.to_list headers
  in
  try
    Eio.Switch.run @@ fun sw ->
    let resp =
      match meth with
      | `GET | `DELETE ->
          Client.request env ~sw ~headers ~meth (Uri.to_string uri)
      | `POST | `PATCH ->
          Client.request env ~sw ~headers ~body:(`Fixed body) ~meth
            (Uri.to_string uri)
      | _ -> failwith "Not implemented method"
    in
    let status = Client.Response.status resp in
    Logq.debug (fun m ->
        m "[fetch] %s %s --> %s" meth_s url
          (Cohttp.Code.string_of_status status));
    let headers = Client.Response.headers resp in
    let body = Client.Response.drain resp in
    Ok (status, headers, body)
  with e ->
    let backtrace = Printexc.get_backtrace () in
    Logq.err (fun m ->
        m "[fetch] %s %s: %s\n%s" meth_s url (Printexc.to_string e) backtrace);
    Error ()

exception FetchFailure of (Yume.Status.t * Yume.Headers.t * string) option

let fetch_exn ?(headers = []) ?(meth = `GET) ?(body = "") ?(sign = None) env
    (url : string) : string =
  match fetch env ~headers ~meth ~body ~sign url with
  | Ok (`OK, _, body) -> body
  | Ok r -> raise (FetchFailure (Some r))
  | _ -> raise (FetchFailure None)

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
  Logq.debug (fun m -> m "Access token for Waq: %s" waq_token);
  new_mastodon_session @@ fun mstdn_token ->
  Logq.debug (fun m -> m "Access token for Mastodon: %s" mstdn_token);
  Unix.sleep 10;
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Time.with_timeout_exn env#clock timeout (fun () ->
      handler env waq_token mstdn_token)

let make_waq_scenario ?(timeout = 30.0) handler () : unit =
  new_session @@ fun waq_token ->
  Logq.debug (fun m -> m "Access token for Waq: %s" waq_token);
  Unix.sleep 1;
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Time.with_timeout_exn env#clock timeout (fun () -> handler env waq_token)

let waq_server_name = Sys.getenv "E2E_TEST_WAQ_SERVER_NAME"
let waq_server_domain = Uri.(of_string waq_server_name |> domain)
let waq url = waq_server_name ^ url
let mstdn_server_name = Sys.getenv "E2E_TEST_MASTODON_SERVER_NAME"
let mstdn_server_domain = Uri.(of_string mstdn_server_name |> domain)
let mstdn url = mstdn_server_name ^ url
let url = function `Waq -> waq | `Mstdn -> mstdn

let pp_json (s : string) =
  Logq.debug (fun m -> m "%s" Yojson.Safe.(from_string s |> pretty_to_string))
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
      Logq.err (fun m ->
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

let get_account_statuses env kind ?token ?(exclude_replies = false) account_id =
  do_fetch env ?token kind
    ("/api/v1/accounts/" ^ account_id ^ "/statuses?exclude_replies="
    ^ string_of_bool exclude_replies)
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
