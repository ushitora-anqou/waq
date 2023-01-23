module Log = Waq.Log
module Uri = Waq.Http.Uri
module Ptime = Waq.Util.Ptime

let ignore_lwt = Waq.Util.ignore_lwt
let fetch = Waq.Http.fetch
let fetch_exn = Waq.Http.fetch_exn

let new_session f =
  let path =
    Sys.getenv_opt "WAQ_BIN" |> Option.value ~default:"test_e2e/launch_waq.sh"
  in
  let open Unix in
  let ic = open_process_args_in path [| path |] in
  let token = In_channel.input_line ic |> Option.value ~default:"" in
  let pid = process_in_pid ic in
  Fun.protect
    (fun () -> f token)
    ~finally:(fun () ->
      kill pid Sys.sigint;
      close_process_in ic |> ignore)

let new_mastodon_session f =
  let path =
    Sys.getenv_opt "MSTDN_BIN"
    |> Option.value ~default:"test_e2e/launch_mstdn.sh"
  in
  let open Unix in
  let ic = open_process_args_in path [| path |] in
  let token = In_channel.input_line ic |> Option.value ~default:"" in
  let pid = process_in_pid ic in
  Fun.protect
    (fun () -> f token)
    ~finally:(fun () ->
      Log.debug (fun m -> m "Killing mastodon processes");
      kill pid Sys.sigint;
      close_process_in ic |> ignore)

let expect_string = function
  | `String s -> s
  | _ -> failwith "Expected string, got something different"

let waq_server_name = Sys.getenv "WAQ_SERVER_NAME"
let waq_server_domain = Uri.(of_string waq_server_name |> domain)
let waq url = waq_server_name ^ url

let mstdn url =
  let server_name = "http://localhost:3000" in
  server_name ^ url

let pp_json (s : string) =
  Log.debug (fun m -> m "%s" Yojson.Safe.(from_string s |> pretty_to_string))
  [@@warning "-32"]

(*
 ========= Scenario 1 =========
*)
let scenario1 waq_token mstdn_token =
  let waq_auth = ("Authorization", "Bearer " ^ waq_token) in
  let mstdn_auth = ("Authorization", "Bearer " ^ mstdn_token) in

  (* Lookup @admin@localhost:3000 *)
  let%lwt r =
    fetch_exn
      (waq "/api/v1/accounts/search?q=@admin@localhost:3000&resolve=true")
  in
  assert (
    let open Yojson.Safe in
    equal (from_string r)
      (from_string
         {|
[{
  "id": "2",
  "username": "admin",
  "acct": "admin@localhost:3000",
  "display_name": ""
}]|}));

  (* Follow @admin@localhost:3000 *)
  fetch_exn ~meth:`POST ~headers:[ waq_auth ] (waq "/api/v1/accounts/2/follow")
  |> ignore_lwt;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Post by @admin@localhost:3000 *)
  let content = "こんにちは、世界！" in
  let%lwt r =
    let body =
      `Assoc [ ("status", `String content) ] |> Yojson.Safe.to_string
    in
    fetch_exn
      ~headers:
        [
          mstdn_auth;
          ("Accept", "application/json");
          ("Content-Type", "application/json");
        ]
      ~meth:`POST ~body (mstdn "/api/v1/statuses")
  in
  let uri, content =
    match Yojson.Safe.from_string r with
    | `Assoc l -> (List.assoc "uri" l, List.assoc "content" l)
    | _ -> assert false
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Post by me *)
  let content2 = "こんにちは、世界！２" in
  let%lwt r =
    let body =
      `Assoc [ ("status", `String content2) ] |> Yojson.Safe.to_string
    in
    fetch_exn
      ~headers:
        [
          waq_auth;
          ("Accept", "application/json");
          ("Content-Type", "application/json");
        ]
      ~meth:`POST ~body (waq "/api/v1/statuses")
  in
  let uri2, content2 =
    match Yojson.Safe.from_string r with
    | `Assoc l -> (List.assoc "uri" l, List.assoc "content" l)
    | _ -> assert false
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Get my home timeline and check *)
  let%lwt r = fetch_exn ~headers:[ waq_auth ] (waq "/api/v1/timelines/home") in
  (match Yojson.Safe.from_string r with
  | `List [ `Assoc l2; `Assoc l ] ->
      (* Check if the timeline is correct *)
      assert (uri = List.assoc "uri" l);
      assert (content = List.assoc "content" l);
      assert (uri2 = List.assoc "uri" l2);
      assert (content2 = List.assoc "content" l2);
      ()
  | _ -> assert false);

  (* Unfollow @admin@localhost:3000 *)
  fetch_exn ~meth:`POST ~headers:[ waq_auth ]
    (waq "/api/v1/accounts/2/unfollow")
  |> ignore_lwt;%lwt
  Lwt_unix.sleep 1.0;%lwt

  (* Get my home timeline and check again *)
  let%lwt r = fetch_exn ~headers:[ waq_auth ] (waq "/api/v1/timelines/home") in
  (match Yojson.Safe.from_string r with
  | `List [ `Assoc l2 ] ->
      (* Check if the timeline is correct *)
      assert (uri2 = List.assoc "uri" l2);
      assert (content2 = List.assoc "content" l2);
      ()
  | _ -> assert false);

  Lwt.return_unit

(*
 ========= Scenario 2 =========
*)
let scenario2 waq_token mstdn_token =
  let waq_auth = ("Authorization", "Bearer " ^ waq_token) in
  let mstdn_auth = ("Authorization", "Bearer " ^ mstdn_token) in

  (* Lookup me from localhost:3000 *)
  let%lwt r =
    fetch_exn ~headers:[ mstdn_auth ]
      (mstdn "/api/v1/accounts/search?q=@foobar@"
      ^ waq_server_domain ^ "&resolve=true")
  in
  let aid =
    match Yojson.Safe.from_string r with
    | `List [ `Assoc l ] -> l |> List.assoc "id" |> expect_string
    | _ -> assert false
  in

  (* Follow me from @admin@localhost:3000 *)
  let%lwt _ =
    fetch_exn ~meth:`POST ~headers:[ mstdn_auth ]
      (mstdn ("/api/v1/accounts/" ^ aid ^ "/follow"))
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Post by @admin@localhost:3000 *)
  let content = "こんにちは、世界！" in
  let%lwt r =
    let body =
      `Assoc [ ("status", `String content) ] |> Yojson.Safe.to_string
    in
    fetch_exn
      ~headers:
        [
          mstdn_auth;
          ("Accept", "application/json");
          ("Content-Type", "application/json");
        ]
      ~meth:`POST ~body (mstdn "/api/v1/statuses")
  in
  let uri, content =
    match Yojson.Safe.from_string r with
    | `Assoc l -> (List.assoc "uri" l, List.assoc "content" l)
    | _ -> assert false
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Post by me *)
  let content2 = "こんにちは、世界！２" in
  let%lwt r =
    let body =
      `Assoc [ ("status", `String content2) ] |> Yojson.Safe.to_string
    in
    fetch_exn
      ~headers:
        [
          waq_auth;
          ("Accept", "application/json");
          ("Content-Type", "application/json");
        ]
      ~meth:`POST ~body (waq "/api/v1/statuses")
  in
  let uri2, content2 =
    match Yojson.Safe.from_string r with
    | `Assoc l -> (List.assoc "uri" l, List.assoc "content" l)
    | _ -> assert false
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 and check *)
  let%lwt r =
    fetch_exn ~headers:[ mstdn_auth ] (mstdn "/api/v1/timelines/home")
  in
  (match Yojson.Safe.from_string r with
  | `List [ `Assoc l2; `Assoc l ] ->
      (* Check if the timeline is correct *)
      assert (uri = List.assoc "uri" l);
      assert (content = List.assoc "content" l);
      assert (uri2 = List.assoc "uri" l2);
      assert (content2 = List.assoc "content" l2);
      ()
  | _ -> assert false);

  (* Unfollow me from @admin@localhost:3000 *)
  let%lwt _ =
    fetch_exn ~meth:`POST ~headers:[ mstdn_auth ]
      (mstdn ("/api/v1/accounts/" ^ aid ^ "/unfollow"))
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 and check again *)
  let%lwt r =
    fetch_exn ~headers:[ mstdn_auth ] (mstdn "/api/v1/timelines/home")
  in
  (match Yojson.Safe.from_string r with
  | `List [ `Assoc l ] ->
      (* Check if the timeline is correct *)
      assert (uri = List.assoc "uri" l);
      assert (content = List.assoc "content" l);
      ()
  | _ -> assert false);

  Lwt.return_unit

let scenario3 _waq_token =
  let%lwt r =
    fetch_exn ~meth:`POST
      ~headers:[ ("Content-Type", "application/json") ]
      ~body:{|{"client_name":"foo","redirect_uris":"http://example.com"}|}
      (waq "/api/v1/apps")
  in
  let client_id, client_secret =
    match Yojson.Safe.from_string r with
    | `Assoc l ->
        ( List.assoc "client_id" l |> expect_string,
          List.assoc "client_secret" l |> expect_string )
    | _ -> assert false
  in

  let%lwt r =
    fetch
      (waq "/oauth/authorize?response_type=code&client_id="
      ^ client_id ^ "&redirect_uri=http://example.com")
  in
  let auth_code =
    match r with
    | Ok (`Found, headers, _body) ->
        (* 0123456789012345678901234
           http://example.com?code=... *)
        let loc = headers |> List.assoc "location" in
        String.sub loc 24 (String.length loc - 24)
    | _ -> assert false
  in

  let%lwt r =
    fetch_exn ~meth:`POST
      ~headers:[ ("Content-Type", "application/json") ]
      ~body:
        (`Assoc
           [
             ("grant_type", `String "authorization_code");
             ("code", `String auth_code);
             ("client_id", `String client_id);
             ("client_secret", `String client_secret);
             ("redirect_uri", `String "http://example.com");
           ]
        |> Yojson.Safe.to_string)
      (waq "/oauth/token")
  in
  let access_token =
    match Yojson.Safe.from_string r with
    | `Assoc l -> l |> List.assoc "access_token" |> expect_string
    | _ -> assert false
  in

  let%lwt r =
    fetch_exn
      ~headers:[ ("Authorization", "Bearer " ^ access_token) ]
      (waq "/api/v1/apps/verify_credentials")
  in
  assert (
    match Yojson.Safe.from_string r with
    | `Assoc l -> l |> List.assoc "name" |> expect_string = "foo"
    | _ -> false);

  Lwt.return_unit

let scenarios_with_waq_and_mstdn () =
  [ (1, scenario1); (2, scenario2) ]
  |> List.iter @@ fun (i, scenario) ->
     Log.debug (fun m -> m "===== Scenario waq-mstdn-%d =====" i);
     new_session @@ fun waq_token ->
     Log.debug (fun m -> m "Access token for Waq: %s" waq_token);
     new_mastodon_session @@ fun mstdn_token ->
     Log.debug (fun m -> m "Access token for Mastodon: %s" mstdn_token);
     Unix.sleep 10;
     Lwt_main.run @@ scenario waq_token mstdn_token

let scenarios_with_waq () =
  [ (1, scenario3) ]
  |> List.iter @@ fun (i, scenario) ->
     Log.debug (fun m -> m "===== Scenario waq-%d =====" i);
     new_session @@ fun waq_token ->
     Log.debug (fun m -> m "Access token for Waq: %s" waq_token);
     Unix.sleep 1;
     Lwt_main.run @@ scenario waq_token

let () =
  print_newline ();
  Log.(add_reporter (make_reporter ~l:Debug ()));
  scenarios_with_waq ();
  scenarios_with_waq_and_mstdn ();
  ()
