type t = {
  listen : string; [@default ""]
  server_name : string; [@default ""]
  db_url : string; [@default ""]
  avatar_url : string; [@default ""]
  header_url : string; [@default ""]
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
let avatar_url () = !c.avatar_url
let header_url () = !c.header_url

let url (l : string list) =
  "https:/" ^ (server_name () :: l |> List.fold_left Util.( ^/ ) "")

let debug_job_kick_block () =
  Sys.getenv_opt "WAQ_DEBUG_JOB_KICK_BLOCK"
  |> Option.map bool_of_string
  |> Option.value ~default:false

let debug_generate_test_users () =
  Sys.getenv_opt "WAQ_GENERATE_TEST_USERS"
  |> Option.map bool_of_string
  |> Option.value ~default:false

let config_path () =
  Sys.getenv_opt "WAQ_CONFIG_PATH" |> Option.value ~default:"config/dev.yml"
