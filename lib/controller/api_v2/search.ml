open Helper
open Lwt.Infix
open Util

type t = {
  accounts : Entity.account list;
  statuses : Entity.status list;
  hashtags : string (* FIXME: dummy *) list;
}
[@@deriving make, yojson]

let parse_query_accounts q =
  let accts = [] in
  let re = Regex.e {|^@?([^@]+)(?:@([^@]+))?$|} in
  try%lwt
    match Regex.match_group re q with
    | Ok [ _; username; "" ] ->
        Activity.search_account (`Webfinger (None, username)) >|= fun a ->
        a :: accts
    | Ok [ _; username; domain ] ->
        Activity.search_account (`Webfinger (Some domain, username))
        >|= fun a -> a :: accts
    | _ -> Lwt.return accts
  with _ -> Lwt.return accts

let parse_query_uri q =
  let try_search_account uri =
    match%lwt Activity.search_account (`Uri uri) with
    | exception _ -> Lwt.return_none
    | a -> Lwt.return_some a
  in
  let try_fetch_status uri =
    match%lwt Activity.fetch_status ~uri with
    | exception _ -> Lwt.return_none
    | s -> Lwt.return_some s
  in
  let list_of_option x = x |> Option.fold ~none:[] ~some:List.singleton in
  let%lwt a_opt = try_search_account q in
  let%lwt s_opt = try_fetch_status q in
  Lwt.return (list_of_option a_opt, list_of_option s_opt)

let parse_query q =
  (* FIXME: Support more kinds of queries *)
  let%lwt a1, s1 = parse_query_uri q in
  let%lwt a2 = parse_query_accounts q in
  let%lwt accounts =
    a1 @ a2
    |> List.map (fun (a : Db.Account.t) -> a#id)
    |> Entity.load_accounts_from_db
  in
  let%lwt statuses =
    s1
    |> List.map (fun (s : Db.Status.t) -> s#id)
    |> Entity.load_statuses_from_db
  in
  let hashtags = [] in
  make ~accounts ~statuses ~hashtags () |> Lwt.return

let get req =
  let%lwt _ = authenticate_user req in
  let q = req |> Httpq.Server.query "q" in
  parse_query q >|= yojson_of_t >>= respond_yojson
