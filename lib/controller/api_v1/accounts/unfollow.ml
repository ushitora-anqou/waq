open Entity
open Lwt.Infix
open Helper
open Util

let service (self : Db.Account.t) (acc : Db.Account.t) (f : Db.Follow.t) =
  Db.Follow.delete ~uri:f.uri ();%lwt
  match acc.domain with
  | None -> Lwt.return_unit
  | Some _ ->
      (* Remote account *)
      let open Activity in
      let obj = make_follow ~id:f.uri ~actor:self.uri ~obj:acc.uri |> follow in
      let activity =
        make_undo
          ~id:(self.uri ^ "#follows" ^/ string_of_int f.id ^/ "undo")
          ~actor:(`String self.uri) ~obj ()
        |> undo
      in
      Worker.Delivery.kick ~activity ~src:self ~dst:acc

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
  if%lwt Lwt.return (f <> None) then service self acc (Option.get f);%lwt

  (* Return the result to the client *)
  make_relationship_from_model self acc
  >|= yojson_of_relationship >>= respond_yojson
