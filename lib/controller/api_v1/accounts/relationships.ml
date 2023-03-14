open Entity
open Lwt.Infix
open Helper

let get req =
  let%lwt self_id = authenticate_user req in
  let%lwt self = Db.(e @@ Account.get_one ~id:self_id) in
  let account_ids =
    req
    |> Httpq.Server.query_many "id[]"
    |> List.map (fun s -> s |> int_of_string |> Model.Account.ID.of_int)
  in
  account_ids
  |> Lwt_list.map_p (fun account_id ->
         Db.e (Model.Account.get_one ~id:account_id)
         >>= make_relationship_from_model self)
  >|= List.map yojson_of_relationship
  >|= (fun l -> `List l)
  >>= respond_yojson
