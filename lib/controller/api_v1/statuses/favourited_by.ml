open Util
open Helper

let get _ req =
  let status_id =
    req |> Yume.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  let accts = Db.(e @@ get_favourited_by ~status_id) in
  let accts =
    accts
    |> List.map (fun (a : Db.Account.t) -> a#id)
    |> Entity.load_accounts_from_db
    |> List.map Entity.yojson_of_account
  in
  `List accts |> respond_yojson
