open Util
open Helper
open Lwt.Infix

let get req =
  let status_id = req |> Httpq.Server.param ":id" |> int_of_string in
  let%lwt accts = Db.get_favourited_by ~status_id in
  let%lwt accts =
    accts
    |> List.map (fun (a : Db.Account.t) -> a.id)
    |> Entity.serialize_accounts
    >|= List.map Entity.yojson_of_account
  in
  `List accts |> respond_yojson
