type t = { listen : string; [@default ""] server_name : string [@default ""] }
[@@deriving make, yaml]

let c = ref (make ())

let load_file fpath =
  c := Yaml_unix.of_file_exn Fpath.(v fpath) |> of_yaml |> Result.get_ok

let listen_host () = Uri.of_string ("//" ^ !c.listen) |> Uri.host |> Option.get
let listen_port () = Uri.of_string ("//" ^ !c.listen) |> Uri.port |> Option.get
let server_name () = !c.server_name

let is_my_domain s =
  (* FIXME: Consider port *)
  let u = !c.server_name |> Uri.of_string in
  s = (Uri.host u |> Option.get)
