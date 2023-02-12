open Entity
open Lwt.Infix
open Helper
open Util

let request_follow ~now ~uri (self : Db.Account.t) (acc : Db.Account.t) =
  (* FIXME: Assume acc is a remote account *)
  assert (acc.domain <> None);

  (* Insert follow_request *)
  Db.FollowRequest.make ~id:0 ~created_at:now ~updated_at:now
    ~account_id:self.id ~target_account_id:acc.id ~uri
  |> Db.FollowRequest.save_one |> ignore_lwt;%lwt

  (* Post activity *)
  let open Activity in
  let activity = make_follow ~id:uri ~actor:self.uri ~obj:acc.uri |> follow in
  Service.Delivery.kick ~activity ~src:self ~dst:acc;

  Lwt.return_unit

let direct_follow ~now ~uri (self : Db.Account.t) (acc : Db.Account.t) =
  (* Assume acc is a local account *)
  assert (acc.domain = None);

  (* Insert follow *)
  Db.Follow.(
    make ~id:0 ~created_at:now ~updated_at:now ~account_id:self.id
      ~target_account_id:acc.id ~uri
    |> save_one)
  |> ignore_lwt

let service (self : Db.Account.t) (acc : Db.Account.t) : unit Lwt.t =
  (* NOTE: Assume there is no follow_request nor follow of (self_id, id) *)
  let now = Ptime.now () in
  let uri = self.uri ^/ Uuidm.(v `V4 |> to_string) in
  match acc.domain with
  | None (* local *) -> direct_follow ~now ~uri self acc
  | Some _ (* remote *) -> request_follow ~now ~uri self acc

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
  (* If valid, follow them *)
  if%lwt Lwt.return (f = None && frq = None) then service self acc;%lwt

  (* Return the result to the client *)
  make_relationship_from_model self acc
  >|= relationship_to_yojson >>= respond_yojson
