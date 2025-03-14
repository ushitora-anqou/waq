(* Entity V1::Instance *)
type v1_instance_urls = { streaming_api : string } [@@deriving make, yojson]

type v1_instance = {
  uri : string;
  title : string;
  short_description : string;
  description : string;
  email : string;
  version : string;
  urls : v1_instance_urls;
      (*
  stats : Yojson.Safe.t;
  thumbnail : string;
  languages : string list;
  registrations : bool;
  approval_required : bool;
  invites_enabled : bool;
  configuration : Yojson.Safe.t;
  contact_account : Yojson.Safe.t;
  rules : Yojson.Safe.t;
  *)
}
[@@deriving make, yojson]

let get _ _req =
  let uri = Config.server_name () in
  let streaming_api = "wss://" ^ uri in
  make_v1_instance ~uri ~title:"Waq"
    ~short_description:"Waq's short description"
    ~description:"Waq's long description" ~email:"admin@example.com"
    ~version:"0.0.1"
    ~urls:(make_v1_instance_urls ~streaming_api)
  |> yojson_of_v1_instance |> Yojson.Safe.to_string
  |> Yume.Server.respond ~headers:[ Helper.content_type_app_json ]
