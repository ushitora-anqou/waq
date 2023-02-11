open Entity
open Lwt.Infix
open Helper

(* Recv POST /api/v1/accounts/:id/follow *)
let post req =
  let%lwt self_id = authenticate_user req in
  let id = req |> Httpq.Server.param ":id" |> int_of_string in

  (* Check if accounts are valid *)
  let%lwt self = Db.Account.get_one ~id:self_id () in
  let%lwt acc = Db.Account.get_one ~id () in

  (* Check if already followed or follow-requested *)
  let%lwt f =
    Db.(
      Follow.get_one ~account_id:self_id ~target_account_id:id ()
      |> maybe_no_row)
  in
  let%lwt frq =
    Db.(
      FollowRequest.get_one ~account_id:self_id ~target_account_id:id ()
      |> maybe_no_row)
  in
  (* If valid, send Follow to the server *)
  if f = None && frq = None then Service.Follow.kick self acc;

  (* Return the result to the client *)
  make_relationship_from_model self acc
  >|= relationship_to_yojson >>= respond_yojson
