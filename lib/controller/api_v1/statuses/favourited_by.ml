open Util
open Helper
open Entity
open Lwt.Infix

let get req =
  let status_id = req |> Httpq.Server.param ":id" |> int_of_string in
  let%lwt accts = Db.get_favourited_by ~status_id in
  let%lwt accts =
    accts
    |> Lwt_list.map_p (fun a -> make_account_from_model a >|= account_to_yojson)
  in
  `List accts |> respond_yojson
