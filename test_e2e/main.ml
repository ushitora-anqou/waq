module Log = Waq.Log
module Uri = Waq.Http.Uri
module Ptime = Waq.Util.Ptime

let ignore_lwt = Waq.Util.ignore_lwt

let fetch ?(headers = []) ?(meth = `GET) ?(body = "") url =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let uri = Uri.of_string url in
  let meth_s = match meth with `GET -> "GET" | `POST -> "POST" in
  let headers =
    let headers =
      ("content-length", body |> String.length |> string_of_int)
      :: ("connection", "close")
      :: ("host", Uri.http_host uri)
      :: ("date", Ptime.(now () |> to_http_date))
      :: headers
    in
    Header.of_list headers
  in
  try%lwt
    let%lwt resp, body =
      match meth with
      | `GET -> Client.get ~headers uri
      | `POST ->
          let body = Cohttp_lwt.Body.of_string body in
          Client.post ~headers ~body uri
    in
    let status = Response.status resp in
    Log.debug (fun m ->
        m "[fetch] %s %s --> %s" meth_s url (Code.string_of_status status));
    let _headers = Response.headers resp in
    let%lwt body = Cohttp_lwt.Body.to_string body in
    Lwt.return_ok (status, body)
  with e ->
    let backtrace = Printexc.get_backtrace () in
    Log.err (fun m ->
        m "[fetch] %s %s: %s\n%s" meth_s url (Printexc.to_string e) backtrace);
    Lwt.return_error ()

let fetch_exn ?(headers = []) ?(meth = `GET) ?(body = "") url =
  match%lwt fetch ~meth ~headers ~body url with
  | Ok (`OK, body) -> Lwt.return body
  | _ -> failwith "fetch_exn failed"

let new_session f =
  let path =
    Sys.getenv_opt "WAQ_BIN" |> Option.value ~default:"test_e2e/launch_waq.sh"
  in
  let open Unix in
  let pid = create_process path [| path |] stdin stdout stderr in
  Fun.protect f ~finally:(fun () -> kill pid Sys.sigint)

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
let scenario1 token =
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
{
  "id": "2",
  "username": "admin",
  "acct": "admin@localhost:3000",
  "display_name": ""
}|}));

  (* Follow @admin@localhost:3000 *)
  fetch_exn ~meth:`POST (waq "/api/v1/accounts/2/follow") |> ignore_lwt;%lwt
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
          ("Authorization", "Bearer " ^ token);
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
        [ ("Accept", "application/json"); ("Content-Type", "application/json") ]
      ~meth:`POST ~body (waq "/api/v1/statuses")
  in
  let uri2, content2 =
    match Yojson.Safe.from_string r with
    | `Assoc l -> (List.assoc "uri" l, List.assoc "content" l)
    | _ -> assert false
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Get my home timeline and check *)
  let%lwt r = fetch_exn (waq "/api/v1/timelines/home") in
  (match Yojson.Safe.from_string r with
  | `List [ `Assoc l2; `Assoc l ] ->
      (* Check if the timeline is correct *)
      assert (uri = List.assoc "uri" l);
      assert (content = List.assoc "content" l);
      assert (uri2 = List.assoc "uri" l2);
      assert (content2 = List.assoc "content" l2);
      ()
  | _ -> assert false);

  Lwt.return_unit

(*
 ========= Scenario 2 =========
*)
let scenario2 token =
  let headers = [ ("Authorization", "Bearer " ^ token) ] in

  (* Lookup me from localhost:3000 *)
  let%lwt r =
    fetch_exn ~headers
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
    fetch_exn ~meth:`POST ~headers
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
          ("Authorization", "Bearer " ^ token);
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
        [ ("Accept", "application/json"); ("Content-Type", "application/json") ]
      ~meth:`POST ~body (waq "/api/v1/statuses")
  in
  let uri2, content2 =
    match Yojson.Safe.from_string r with
    | `Assoc l -> (List.assoc "uri" l, List.assoc "content" l)
    | _ -> assert false
  in
  Lwt_unix.sleep 1.0;%lwt

  (* Get home timeline of @admin@localhost:3000 and check *)
  let%lwt r = fetch_exn ~headers (mstdn "/api/v1/timelines/home") in
  (match Yojson.Safe.from_string r with
  | `List [ `Assoc l2; `Assoc l ] ->
      (* Check if the timeline is correct *)
      assert (uri = List.assoc "uri" l);
      assert (content = List.assoc "content" l);
      assert (uri2 = List.assoc "uri" l2);
      assert (content2 = List.assoc "content" l2);
      ()
  | _ -> assert false);
  Lwt.return_unit

let () =
  print_newline ();
  Log.(add_reporter (make_reporter ~l:Debug ()));
  [ (1, scenario1); (2, scenario2) ]
  |> List.iter @@ fun (i, scenario) ->
     Log.debug (fun m -> m "===== Scenario %d =====" i);
     new_session @@ fun () ->
     new_mastodon_session @@ fun token ->
     Log.debug (fun m -> m "Access token for Mastodon: %s" token);
     Unix.sleep 10;
     Lwt_main.run @@ scenario token
