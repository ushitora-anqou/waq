open Helper
open Lwt.Infix
open Util

type t = {
  accounts : Entity.account list;
  statuses : Entity.status list;
  hashtags : string (* FIXME: dummy *) list;
}
[@@deriving make, yojson_of]

let handle_query_accounts resolve q =
  let re = Regex.e {|^@?([^@]+)(?:@([^@]+))?$|} in
  try%lwt
    match Regex.match_ re q with
    | [ [| _; Some { substr = username; _ }; None |] ] -> (
        match%lwt Db.(e Account.(select_by_username_prefix username)) with
        | [] ->
            Activity.search_account ~resolve (`Webfinger (None, username))
            >|= fun a -> [ a ]
        | xs -> Lwt.return xs)
    | [ [| _; Some username; Some domain |] ] ->
        Activity.search_account ~resolve
          (`Webfinger (Some domain.substr, username.substr))
        >|= fun a -> [ a ]
    | _ -> Lwt.return []
  with _ -> Lwt.return []

let handle_query_uri resolve q =
  let try_search_account uri =
    match%lwt Activity.search_account ~resolve (`Uri uri) with
    | exception e ->
        Logq.debug (fun m ->
            m "try_search_account failed: %s\n%s" (Printexc.to_string e)
              (Printexc.get_backtrace ()));
        Lwt.return_none
    | a -> Lwt.return_some a
  in
  let try_fetch_status uri =
    match%lwt Activity.fetch_status ~uri with
    | exception e ->
        Logq.debug (fun m ->
            m "try_fetch_status failed: %s\n%s" (Printexc.to_string e)
              (Printexc.get_backtrace ()));
        Lwt.return_none
    | s -> Lwt.return_some s
  in
  let list_of_option x = x |> Option.fold ~none:[] ~some:List.singleton in
  if
    String.(starts_with ~prefix:"http://" q || starts_with ~prefix:"https://" q)
  then
    let%lwt a_opt = try_search_account q in
    let%lwt s_opt = if resolve then try_fetch_status q else Lwt.return_none in
    Lwt.return (list_of_option a_opt, list_of_option s_opt)
  else Lwt.return ([], [])

let handle_query resolve q =
  (* FIXME: Support more kinds of queries *)
  let q = String.trim q in
  let%lwt a1, s1 = handle_query_uri resolve q in
  let%lwt a2 = handle_query_accounts resolve q in
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
  let%lwt self = may_authenticate_account req in
  let%lwt q = req |> Httpq.Server.query "q" in
  Logq.debug (fun m -> m "[/api/v2/search] %b %s" (Option.is_some self) q);
  handle_query (Option.is_some self) q >|= yojson_of_t >>= respond_yojson
