open Entity
open Helper

let get _ req =
  let id =
    req |> Yume.Server.param ":id" |> int_of_string |> Model.Account.ID.of_int
  in
  match Db.e (Model.Account.get_one ~id) with
  | exception Sqlx.Error.NoRowFound ->
      Yume.Server.raise_error_response `Not_found
  | a -> make_account_from_model a |> yojson_of_account |> respond_yojson
