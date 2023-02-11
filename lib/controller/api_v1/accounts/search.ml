open Activity

(* Recv GET /api/v1/accounts/search *)
type entry = {
  id : string;
  username : string;
  acct : string;
  display_name : string;
}
[@@deriving make, yojson]

type t = entry list [@@deriving yojson]

let parse_req req =
  let open Httpq.Server in
  let open Helper in
  let resolve = req |> query ~default:"false" "resolve" |> bool_of_string in
  let username, domain = req |> query "q" |> parse_webfinger_address in
  (resolve, username, domain)

let get req =
  let _resolve, username, domain = parse_req req in
  try%lwt
    let%lwt acc = fetch_account (`Webfinger (domain, username)) in
    let acct =
      match acc.domain with
      | None -> username
      | Some domain -> username ^ "@" ^ domain
    in
    let ent =
      make_entry ~id:(string_of_int acc.id) ~username ~acct
        ~display_name:acc.display_name
    in
    [ ent ] |> to_yojson |> Yojson.Safe.to_string
    |> Httpq.Server.respond ~headers:[ Helper.content_type_app_json ]
  with _ -> Httpq.Server.raise_error_response `Not_found
