open Entity
open Lwt.Infix
open Helper

(* Recv POST /api/v1/accounts/:id/unfollow *)
let post req =
  let%lwt self_id = authenticate_user req in
  let id = req |> Httpq.Server.param ":id" |> int_of_string in

  (* Check if accounts are valid *)
  let%lwt self = Db.Account.get_one ~id:self_id () in
  let%lwt acc = Db.Account.get_one ~id () in

  (* Check if followed *)
  let%lwt f =
    Db.(
      Follow.get_one ~account_id:self_id ~target_account_id:id ()
      |> maybe_no_row)
  in
  (* If valid, send Undo of Follow to the server *)
  if f <> None then Service.Unfollow.kick self acc (Option.get f);

  (* Return the result to the client *)
  make_relationship_from_model self acc
  >|= relationship_to_yojson >>= respond_yojson
