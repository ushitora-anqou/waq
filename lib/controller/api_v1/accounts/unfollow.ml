open Entity
open Helper
open Util

let service env (self : Db.Account.t) (acc : Db.Account.t) (f : Db.Follow.t) =
  Db.(e @@ Follow.delete [ f ]);
  match acc#domain with
  | None -> ()
  | Some _ ->
      (* Remote account *)
      let open Activity in
      let obj = make_follow ~id:f#uri ~actor:self#uri ~obj:acc#uri |> follow in
      let activity =
        make_undo
          ~id:
            (self#uri ^ "#follows"
            ^/ (f#id |> Model.Follow.ID.to_int |> string_of_int)
            ^/ "undo")
          ~actor:(`String self#uri) ~obj ()
        |> undo
      in
      Worker.Delivery.kick env ~activity ~src:self ~url:acc#inbox_url

(* Recv POST /api/v1/accounts/:id/unfollow *)
let post env req =
  let self = authenticate_account req in
  let id = req |> Yume.Server.param ":id" |> string_to_account_id in

  (* Check if accounts are valid *)
  let acc = Db.e (Model.Account.get_one ~id) in

  (* Check if followed *)
  let f =
    try Some Db.(e @@ Follow.get_one ~account_id:self#id ~target_account_id:id)
    with Sqlx.Error.NoRowFound -> None
  in
  (* If valid, send Undo of Follow to the server *)
  if f <> None then service env self acc (Option.get f);

  (* Return the result to the client *)
  make_relationship_from_model self acc
  |> yojson_of_relationship |> respond_yojson
