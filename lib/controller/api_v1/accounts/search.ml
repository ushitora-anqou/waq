open Lwt.Infix
open Activity
open Helper

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
  let%lwt resolve =
    req |> query ~default:"false" "resolve" >|= bool_of_string
  in
  req |> query "q" >|= parse_webfinger_address >|= fun (username, domain) ->
  (resolve, username, domain)

let get req =
  let%lwt _ = authenticate_bearer req in
  let%lwt _resolve, username, domain = parse_req req in
  try%lwt
    let%lwt acc = search_account (`Webfinger (domain, username)) in
    let acct =
      match acc#domain with
      | None -> username
      | Some domain -> username ^ "@" ^ domain
    in
    let ent =
      make_entry
        ~id:(acc#id |> Model.Account.ID.to_int |> string_of_int)
        ~username ~acct ~display_name:acc#display_name
    in
    [ ent ] |> yojson_of_t |> Yojson.Safe.to_string
    |> Httpq.Server.respond ~headers:[ Helper.content_type_app_json ]
  with _ -> Httpq.Server.raise_error_response `Not_found
