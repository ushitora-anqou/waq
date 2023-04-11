include Common

type agent = {
  kind : [ `Waq | `Mstdn ];
  token : string;
  username : string;
  domain : string;
}
[@@deriving make]

let acct_of_agent ~(from : agent) (a : agent) =
  if a.domain = from.domain then a.username else a.username ^ "@" ^ a.domain

let lookup src = lookup src.kind ~token:src.token
let follow src = follow src.kind ~token:src.token
let post src = post src.kind ~token:src.token
let search src = search src.kind ~token:src.token
let reblog src = reblog src.kind ~token:src.token
let unreblog src = unreblog src.kind ~token:src.token

let home_timeline src =
  home_timeline src.kind ~token:src.token >|= List.map status_of_yojson

let delete_status src = delete_status src.kind ~token:src.token
let get_status src = get_status src.kind ~token:src.token
let get_notifications src = get_notifications src.kind ~token:src.token

let websocket src ?target handler f =
  websocket src.kind ~token:src.token ?target handler f

let upload_media src ~filename ~data ~content_type =
  let target = "/api/v2/media" in
  let headers =
    [
      (`Accept, "application/json");
      (`Authorization, "Bearer " ^ src.token);
      ( `Content_type,
        "multipart/form-data; \
         boundary=---------------------------91791948726096252761377705945" );
    ]
  in
  let body =
    [ {|-----------------------------91791948726096252761377705945--|}; {||} ]
  in
  let body =
    [
      {|-----------------------------91791948726096252761377705945|};
      {|Content-Disposition: form-data; name="file"; filename="|} ^ filename
      ^ {|"|};
      {|Content-Type: |} ^ content_type;
      {||};
      data;
    ]
    @ body
  in
  assert (List.length body <> 2);
  let body = String.concat "\r\n" body in
  fetch_exn ~headers ~meth:`POST ~body (url src.kind target)
  >|= Yojson.Safe.from_string >|= media_attachment_of_yojson

let lookup_agent src dst =
  let domain = if src.domain = dst.domain then None else Some dst.domain in
  lookup src ~username:dst.username ?domain ()

let follow_agent src dst =
  let%lwt id, _, _ = lookup_agent src dst in
  follow src id

let expect_no_status src id =
  try%lwt
    get_status src id |> ignore_lwt;%lwt
    assert false
  with Httpq.Client.FetchFailure (Some (`Not_found, _, _)) -> Lwt.return_unit

type runtime_context = {
  waq_tokens : string array;
  mutable waq_num_used_tokens : int;
  mstdn_tokens : string array;
  mutable mstdn_num_used_tokens : int;
}
[@@deriving make]

let generate_waq_agent ctxt =
  let i = ctxt.waq_num_used_tokens in
  ctxt.waq_num_used_tokens <- i + 1;
  let token = ctxt.waq_tokens.(i) in
  let username = "user" ^ string_of_int (i + 1) in
  make_agent ~kind:`Waq ~token ~username ~domain:waq_server_domain

let generate_mstdn_agent ctxt =
  let i = ctxt.mstdn_num_used_tokens in
  ctxt.mstdn_num_used_tokens <- i + 1;
  let token = ctxt.mstdn_tokens.(i) in
  let username = "mstdn" ^ string_of_int (i + 1) in
  make_agent ~kind:`Mstdn ~token ~username ~domain:"localhost:3000"

let new_session f =
  let path =
    Sys.getenv_opt "WAQ_BIN" |> Option.value ~default:"test_e2e/launch_waq.sh"
  in
  let open Unix in
  let ic = open_process_args_in path [| path |] in
  let token1 = In_channel.input_line ic |> Option.get in
  let token2 = In_channel.input_line ic |> Option.get in
  let token3 = In_channel.input_line ic |> Option.get in
  let pid = process_in_pid ic in
  Fun.protect
    (fun () -> f [| token1; token2; token3 |])
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
  let _admin = In_channel.input_line ic |> Option.get in
  let token1 = In_channel.input_line ic |> Option.get in
  let token2 = In_channel.input_line ic |> Option.get in
  let token3 = In_channel.input_line ic |> Option.get in
  let pid = process_in_pid ic in
  Fun.protect
    (fun () -> f [| token1; token2; token3 |])
    ~finally:(fun () ->
      Logq.debug (fun m -> m "Killing mastodon processes");
      kill pid Sys.sigint;
      close_process_in ic |> ignore)

let launch_waq ?(timeout = 30.0) (f : runtime_context -> unit Lwt.t) : unit =
  new_session @@ fun waq_tokens ->
  Logq.debug (fun m ->
      m "Access token for Waq: [%s]"
        (waq_tokens |> Array.to_list |> String.concat ";"));
  let ctxt =
    make_runtime_context ~waq_tokens ~mstdn_tokens:[||] ~waq_num_used_tokens:0
      ~mstdn_num_used_tokens:0
  in
  Unix.sleep 10;
  Lwt.pick [ f ctxt; (Lwt_unix.sleep timeout >>= fun () -> failwith "Timeout") ]
  |> Lwt_main.run

let launch_waq_and_mstdn ?(timeout = 30.0) (f : runtime_context -> unit Lwt.t) :
    unit =
  new_session @@ fun waq_tokens ->
  Logq.debug (fun m ->
      m "Access token for Waq: [%s]"
        (waq_tokens |> Array.to_list |> String.concat ";"));
  new_mastodon_session @@ fun mstdn_tokens ->
  Logq.debug (fun m ->
      m "Access token for Mastodon: [%s]"
        (mstdn_tokens |> Array.to_list |> String.concat ";"));
  let ctxt =
    make_runtime_context ~waq_tokens ~mstdn_tokens ~waq_num_used_tokens:0
      ~mstdn_num_used_tokens:0
  in
  Unix.sleep 10;
  Lwt.pick [ f ctxt; (Lwt_unix.sleep timeout >>= fun () -> failwith "Timeout") ]
  |> Lwt_main.run
