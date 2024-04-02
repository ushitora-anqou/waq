module Internal = struct
  type cache = { m : (string, string) Hashtbl.t }

  let cache = { m = Hashtbl.create 10 }

  let getenv ~default name =
    match Hashtbl.find_opt cache.m name with
    | Some v -> v
    | None -> Sys.getenv_opt name |> Option.value ~default

  let setenv name value = Hashtbl.replace cache.m name value
  let listen () = getenv ~default:"127.0.0.1:8000" "LISTEN"
  let server_name () = getenv ~default:"" "SERVER_NAME"
  let db_url () = getenv ~default:"" "DB_URL"

  let not_found_avatar_url () =
    getenv ~default:"/avatars/original/missing.png" "NOT_FOUND_AVATAR_URL"

  let not_found_header_url () =
    getenv ~default:"/headers/original/missing.png" "NOT_FOUND_HEADER_URL"

  let default_avatar_url () =
    getenv ~default:"/avatars/original/missing.png" "DEFAULT_AVATAR_URL"

  let default_header_url () =
    getenv ~default:"/headers/original/missing.png" "DEFAULT_HEADER_URL"

  let static_root () = getenv ~default:"static" "STATIC_ROOT"
  let vapid_private_key () = getenv ~default:"" "VAPID_PRIVATE_KEY"
  let vapid_public_key () = getenv ~default:"" "VAPID_PUBLIC_KEY"
  let webpush_subscriber () = getenv ~default:"" "WEBPUSH_SUBSCRIBER"
  let log_file_path () = getenv ~default:"" "LOG_FILE_PATH"
end

let listen_host () =
  Uri.of_string ("//" ^ Internal.listen ()) |> Uri.host |> Option.get

let listen_port () =
  Uri.of_string ("//" ^ Internal.listen ()) |> Uri.port |> Option.get

let server_name () = Internal.server_name ()

let is_my_domain s =
  (* FIXME: Consider port *)
  s = server_name ()

let db_url () = Internal.db_url ()
let static_root () = Internal.static_root ()

let url (l : string list) =
  "https:/" ^ (server_name () :: l |> List.fold_left Util.( ^/ ) "")

let absolute_url (src : string) =
  let open Uri in
  let u = of_string src in
  let u = with_scheme u (Some "https") in
  let u = with_host u (Some (server_name ())) in
  to_string u

let default_avatar_url () = absolute_url (Internal.default_avatar_url ())
let default_header_url () = absolute_url (Internal.default_header_url ())
let notfound_avatar_url () = absolute_url (Internal.not_found_avatar_url ())
let notfound_header_url () = absolute_url (Internal.not_found_header_url ())

let account_avatar_dir () =
  [ "system"; "accounts"; "avatars" ]
  |> List.fold_left Filename.concat (static_root ())

let account_avatar_url filename =
  "/" ^ ([ "system"; "accounts"; "avatars"; filename ] |> String.concat "/")
  |> absolute_url

let account_header_dir () =
  [ "system"; "accounts"; "headers" ]
  |> List.fold_left Filename.concat (static_root ())

let account_header_url filename =
  "/" ^ ([ "system"; "accounts"; "headers"; filename ] |> String.concat "/")
  |> absolute_url

let vapid_private_key () = Internal.vapid_private_key ()
let vapid_public_key () = Internal.vapid_public_key ()
let webpush_subscriber () = Internal.webpush_subscriber ()
let log_file_path () = Internal.log_file_path ()

let debug_job_kick_block () =
  Sys.getenv_opt "WAQ_DEBUG_JOB_KICK_BLOCK"
  |> Option.map bool_of_string
  |> Option.value ~default:false

let debug_generate_test_users () =
  Sys.getenv_opt "WAQ_GENERATE_TEST_USERS"
  |> Option.map bool_of_string
  |> Option.value ~default:false

let debug_dump_req_dir () = Sys.getenv_opt "WAQ_DUMP_REQ_DIR"

let debug_no_throttle_fetch () =
  match
    Sys.getenv_opt "WAQ_NO_THROTTLE_FETCH" |> Option.map String.lowercase_ascii
  with
  | Some ("true" | "1") -> true
  | _ -> false

let to_list () =
  Internal.
    [
      ("listen", listen ());
      ("server_name", server_name ());
      ("db_url", db_url ());
      ("not_found_avatar_url", not_found_avatar_url ());
      ("not_found_header_url", not_found_header_url ());
      ("default_avatar_url", default_avatar_url ());
      ("default_header_url", default_header_url ());
      ("static_root", static_root ());
      ("vapid_private_key", vapid_private_key ());
      ("vapid_public_key", vapid_public_key ());
      ("webpush_subscriber", webpush_subscriber ());
      ("log_file_path", log_file_path ());
    ]

let verify_for_server () =
  if server_name () = "" then Error "server_name is not set"
  else if db_url () = "" then Error "db_url is not set"
  else if vapid_private_key () = "" then Error "vapid_private_key is not set"
  else if vapid_public_key () = "" then Error "vapid_public_key is not set"
  else Ok ()
