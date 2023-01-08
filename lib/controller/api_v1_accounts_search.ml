open Common
open Activity

(* Recv GET /api/v1/accounts/search *)
type get_api_v1_accounts_search_res = {
  id : string;
  username : string;
  acct : string;
  display_name : string;
}
[@@deriving make, yojson { strict = false }]

let get _resolve ~username ~domain =
  try%lwt
    let%lwt acc = fetch_account (`Webfinger (domain, username)) in
    let acct =
      match acc.domain with
      | None -> username
      | Some _ -> username ^ "@" ^ domain
    in
    make_get_api_v1_accounts_search_res ~id:(string_of_int acc.id) ~username
      ~acct ~display_name:acc.display_name
    |> get_api_v1_accounts_search_res_to_yojson |> Yojson.Safe.to_string
    |> Result.ok |> Lwt.return
  with _ -> Lwt.return (Error `Not_found)
