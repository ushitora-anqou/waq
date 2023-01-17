type account = {
  id : string;
  username : string;
  acct : string;
  display_name : string;
  created_at : string;
}
[@@deriving make, yojson]

type status = {
  id : string;
  created_at : string;
  visibility : string;
  uri : string;
  content : string;
  account : account;
}
[@@deriving make, yojson]

let acct (username : string) (domain : string option) : string =
  match domain with None -> username | Some domain -> username ^ "@" ^ domain

let get ~self_id ~max_id ~since_id ~limit =
  let conv (s : Db.Status.t) =
    let%lwt a = Db.Account.get ~by:(`id s.account_id) in
    let account =
      make_account ~id:(string_of_int a.id) ~username:a.username
        ~acct:(acct a.username a.domain) ~display_name:a.display_name
        ~created_at:(Ptime.to_rfc3339 a.created_at)
    in
    make_status ~id:(string_of_int s.id)
      ~created_at:(Ptime.to_rfc3339 s.created_at)
      ~visibility:"public" ~uri:s.uri ~content:s.text ~account
    |> status_to_yojson |> Lwt.return
  in
  let%lwt statuses = Db.home_timeline ~id:self_id ~limit ~max_id ~since_id in
  let%lwt statuses = Lwt_list.map_p conv statuses in
  `List statuses |> Yojson.Safe.to_string |> Result.ok |> Lwt.return
