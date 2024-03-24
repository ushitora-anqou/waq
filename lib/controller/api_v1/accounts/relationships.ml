open Entity
open Helper

let get _ req =
  let self = authenticate_account req in
  let account_ids =
    req
    |> Yume.Server.query_many "id"
    |> List.map (fun s -> s |> int_of_string |> Model.Account.ID.of_int)
  in
  account_ids
  |> List.map (fun account_id ->
         Db.e (Model.Account.get_one ~id:account_id)
         |> make_relationship_from_model self)
  |> List.map yojson_of_relationship
  |> (fun l -> `List l)
  |> respond_yojson
