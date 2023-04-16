type t = {
  listen : string; [@default ""]
  server_name : string; [@default ""]
  db_url : string; [@default ""]
  notfound_avatar_url : string; [@default ""]
  notfound_header_url : string; [@default ""]
  default_avatar_url : string; [@default ""]
  default_header_url : string; [@default ""]
  static_root : string; [@default ""]
  vapid_private_key : string; [@default ""]
  vapid_public_key : string; [@default ""]
}
[@@deriving make, yaml]

let c = ref (make ())
let load_string s = c := s |> Yaml.of_string_exn |> of_yaml |> Result.get_ok

let load_file fpath =
  c := Yaml_unix.of_file_exn Fpath.(v fpath) |> of_yaml |> Result.get_ok

let listen_host () = Uri.of_string ("//" ^ !c.listen) |> Uri.host |> Option.get
let listen_port () = Uri.of_string ("//" ^ !c.listen) |> Uri.port |> Option.get
let server_name () = !c.server_name

let is_my_domain s =
  (* FIXME: Consider port *)
  s = !c.server_name

let db_url () = !c.db_url
let static_root () = !c.static_root

let url (l : string list) =
  "https:/" ^ (server_name () :: l |> List.fold_left Util.( ^/ ) "")

let absolute_url (src : string) =
  let open Uri in
  let u = of_string src in
  let u = with_scheme u (Some "https") in
  let u = with_host u (Some (server_name ())) in
  to_string u

let default_avatar_url () = absolute_url !c.default_avatar_url
let default_header_url () = absolute_url !c.default_header_url
let notfound_avatar_url () = absolute_url !c.notfound_avatar_url
let notfound_header_url () = absolute_url !c.notfound_header_url

let media_attachment_path filename =
  [ "system"; "media_attachments"; filename ]
  |> List.fold_left Filename.concat (static_root ())

let media_attachment_url filename =
  "/" ^ ([ "system"; "media_attachments"; filename ] |> String.concat "/")
  |> absolute_url

let vapid_private_key () = !c.vapid_private_key
let vapid_public_key () = !c.vapid_public_key

let config_path () =
  Sys.getenv_opt "WAQ_CONFIG_PATH" |> Option.value ~default:"config/dev.yml"

let debug_job_kick_block () =
  Sys.getenv_opt "WAQ_DEBUG_JOB_KICK_BLOCK"
  |> Option.map bool_of_string
  |> Option.value ~default:false

let debug_generate_test_users () =
  Sys.getenv_opt "WAQ_GENERATE_TEST_USERS"
  |> Option.map bool_of_string
  |> Option.value ~default:false

let debug_dump_req_dir () = Sys.getenv_opt "WAQ_DUMP_REQ_DIR"
