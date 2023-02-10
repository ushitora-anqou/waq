open Util
open Helper
open Entity

let get req =
  let status_id = req |> Httpq.Server.param ":id" |> int_of_string in
  let%lwt accts = Db.get_favourited_by ~status_id in
  let accts =
    accts |> List.map (make_account_from_model |.> account_to_yojson)
  in
  `List accts |> respond_yojson
