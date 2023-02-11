open Entity
open Lwt.Infix
open Helper

let get req =
  let%lwt self_id = authenticate_user req in
  let account_ids =
    req |> Httpq.Server.query_many "id[]" |> List.map int_of_string
  in
  let%lwt self = Db.Account.get_one ~id:self_id () in
  account_ids
  |> Lwt_list.map_p (fun account_id ->
         Db.Account.get_one ~id:account_id ()
         >>= make_relationship_from_model self)
  >|= List.map relationship_to_yojson
  >|= (fun l -> `List l)
  >>= respond_yojson
