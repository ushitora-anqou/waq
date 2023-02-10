open Entity
open Helper
open Lwt.Infix

type t = {
  accounts : account list;
  statuses : status list;
  hashtags : Yojson.Safe.t list;
}
[@@deriving make, yojson]

let parse_query_accounts q =
  let accts = [] in
  let re = Regex.e {|^@?([^@]+)(?:@([^@]+))?$|} in
  match Regex.match_group re q with
  | Ok [ _; username; "" ] ->
      Activity.fetch_account (`Webfinger (None, username)) >|= fun a ->
      a :: accts
  | Ok [ _; username; domain ] ->
      Activity.fetch_account (`Webfinger (Some domain, username)) >|= fun a ->
      a :: accts
  | _ -> Lwt.return accts

let parse_query q =
  (* FIXME: Support more kinds of queries *)
  let%lwt accounts =
    parse_query_accounts q >|= List.map make_account_from_model
  in
  let statuses = [] in
  let hashtags = [] in
  make ~accounts ~statuses ~hashtags () |> Lwt.return

let get req =
  let%lwt _ = authenticate_user req in
  let q = req |> Httpq.Server.query "q" in
  parse_query q >|= to_yojson >>= respond_yojson
