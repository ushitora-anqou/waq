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

let lookup env src = lookup env src.kind ~token:src.token
let follow env src = follow env src.kind ~token:src.token
let post env src = post env src.kind ~token:src.token
let search env src = search env src.kind ~token:src.token
let reblog env src = reblog env src.kind ~token:src.token
let unreblog env src = unreblog env src.kind ~token:src.token

let lookup_via_v1_accounts_lookup env src =
  lookup_via_v1_accounts_lookup env src.kind ~token:src.token

let home_timeline env src =
  home_timeline env src.kind ~token:src.token |> List.map status_of_yojson

let delete_status env src = delete_status env src.kind ~token:src.token
let get_status env src = get_status env src.kind ~token:src.token
let get_notifications env src = get_notifications env src.kind ~token:src.token

let get_account_statuses env src =
  get_account_statuses env src.kind ~token:src.token

let get_status_context env src = get_status_context env src.kind

let update_credentials env src =
  update_credentials env src.kind ~token:src.token

let websocket env src ?target handler f =
  websocket env src.kind ~token:src.token ?target handler f

let upload_media env src ~filename ~data ~content_type =
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
  fetch_exn env ~headers ~meth:`POST ~body (url src.kind target)
  |> Yojson.Safe.from_string |> media_attachment_of_yojson

let lookup_agent env src dst =
  let domain = if src.domain = dst.domain then None else Some dst.domain in
  lookup env src ~username:dst.username ?domain ()

let follow_agent env src dst =
  let id, _, _ = lookup_agent env src dst in
  follow env src id

let expect_no_status env src id =
  try
    get_status env src id |> ignore;
    assert false
  with Yume.Client.FetchFailure (Some (`Not_found, _, _)) -> ()

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
  make_agent ~kind:`Mstdn ~token ~username ~domain:mstdn_server_domain

let launch_waq ?(timeout = 30.0)
    (f : Eio_unix.Stdenv.base -> runtime_context -> unit) : unit =
  Internal.launch_waq @@ fun waq_tokens ->
  Logs.debug (fun m ->
      m "Access token for Waq: [%s]"
        (waq_tokens |> Array.to_list |> String.concat ";"));
  let ctxt =
    make_runtime_context ~waq_tokens ~mstdn_tokens:[||] ~waq_num_used_tokens:0
      ~mstdn_num_used_tokens:0
  in
  Unix.sleep 10;
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  Eio.Time.with_timeout_exn env#clock timeout (fun () -> f env ctxt)

let launch_waq_and_mstdn ?(timeout = 30.0)
    (f : Eio_unix.Stdenv.base -> runtime_context -> unit) : unit =
  Internal.launch_waq @@ fun waq_tokens ->
  Logs.debug (fun m ->
      m "Access token for Waq: [%s]"
        (waq_tokens |> Array.to_list |> String.concat ";"));
  Internal.launch_mastodon @@ fun mstdn_tokens ->
  Logs.debug (fun m ->
      m "Access token for Mastodon: [%s]"
        (mstdn_tokens |> Array.to_list |> String.concat ";"));
  let ctxt =
    make_runtime_context ~waq_tokens ~mstdn_tokens ~waq_num_used_tokens:0
      ~mstdn_num_used_tokens:0
  in
  Unix.sleep 10;
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  Eio.Time.with_timeout_exn env#clock timeout (fun () -> f env ctxt)
