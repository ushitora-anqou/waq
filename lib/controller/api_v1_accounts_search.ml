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

let get _resolve ~username ~domain =
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
    [ ent ] |> to_yojson |> Yojson.Safe.to_string |> Result.ok |> Lwt.return
  with _ -> Lwt.return (Error `Not_found)
