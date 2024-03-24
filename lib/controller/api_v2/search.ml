open Helper
open Util

type t = {
  accounts : Entity.account list;
  statuses : Entity.status list;
  hashtags : string (* FIXME: dummy *) list;
}
[@@deriving make, yojson_of]

let handle_query_accounts env resolve q =
  let re = Regex.e {|^@?([^@]+)(?:@([^@]+))?$|} in
  try
    match Regex.match_ re q with
    | [ [| _; Some { substr = username; _ }; None |] ] -> (
        match Db.(e Account.(select_by_username_prefix username)) with
        | [] ->
            Activity.search_account env ~resolve (`Webfinger (None, username))
            |> fun a -> [ a ]
        | xs -> xs)
    | [ [| _; Some username; Some domain |] ] ->
        Activity.search_account env ~resolve
          (`Webfinger (Some domain.substr, username.substr))
        |> fun a -> [ a ]
    | _ -> []
  with _ -> []

let handle_query_uri env resolve q =
  let try_search_account uri =
    try Some (Activity.search_account env ~resolve (`Uri uri))
    with e ->
      Logq.debug (fun m ->
          m "try_search_account failed: %s\n%s" (Printexc.to_string e)
            (Printexc.get_backtrace ()));
      None
  in
  let try_fetch_status uri =
    try Some (Activity.fetch_status env ~uri)
    with e ->
      Logq.debug (fun m ->
          m "try_fetch_status failed: %s\n%s" (Printexc.to_string e)
            (Printexc.get_backtrace ()));
      None
  in
  let list_of_option x = x |> Option.fold ~none:[] ~some:List.singleton in
  if
    String.(starts_with ~prefix:"http://" q || starts_with ~prefix:"https://" q)
  then
    let a_opt = try_search_account q in
    let s_opt = if resolve then try_fetch_status q else None in
    (list_of_option a_opt, list_of_option s_opt)
  else ([], [])

let handle_query env resolve q =
  (* FIXME: Support more kinds of queries *)
  let q = String.trim q in
  let a1, s1 = handle_query_uri env resolve q in
  let a2 = handle_query_accounts env resolve q in
  let accounts =
    a1 @ a2
    |> List.map (fun (a : Db.Account.t) -> a#id)
    |> Entity.load_accounts_from_db
  in
  let statuses =
    s1
    |> List.map (fun (s : Db.Status.t) -> s#id)
    |> Entity.load_statuses_from_db
  in
  let hashtags = [] in
  make ~accounts ~statuses ~hashtags ()

let get env req =
  let self = may_authenticate_account req in
  let q = req |> Yume.Server.query "q" in
  Logq.debug (fun m -> m "[/api/v2/search] %b %s" (Option.is_some self) q);
  handle_query env (Option.is_some self) q |> yojson_of_t |> respond_yojson
