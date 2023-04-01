open Entity
open Helper
open Util

let request_follow ~uri (self : Db.Account.t) (acc : Db.Account.t) =
  (* FIXME: Assume acc is a remote account *)
  assert (acc#domain <> None);

  (* Insert follow_request *)
  Db.(
    e
    @@ FollowRequest.(
         make ~account_id:self#id ~target_account_id:acc#id ~uri () |> save_one))
  |> ignore_lwt;%lwt

  (* Post activity *)
  let open Activity in
  let activity = make_follow ~id:uri ~actor:self#uri ~obj:acc#uri |> follow in
  Worker.Delivery.kick ~activity ~src:self ~url:acc#inbox_url;%lwt

  Lwt.return_unit

let direct_follow ~uri (self : Db.Account.t) (acc : Db.Account.t) =
  (* Assume acc is a local account *)
  assert (acc#domain = None);

  (* Insert follow *)
  let%lwt f =
    Db.(
      e
      @@ Follow.(
           make ~account_id:self#id ~target_account_id:acc#id ~uri ()
           |> save_one))
  in

  (* Notify *)
  Worker.Local_notify.kick
    ~activity_id:(Model.Follow.ID.to_int f#id)
    ~activity_type:`Follow ~dst:acc ~src:self ~typ:`follow;%lwt

  Lwt.return_unit

let follow_not_possible ~(src : Db.Account.t) ~(dst : Db.Account.t) : bool Lwt.t
    =
  Lwt.return (src#id = dst#id)

let already_followed ~(src : Db.Account.t) ~(dst : Db.Account.t) : bool Lwt.t =
  match%lwt
    Db.(e @@ Follow.get_one ~account_id:src#id ~target_account_id:dst#id)
  with
  | _ -> Lwt.return_true
  | exception Sqlx.Error.NoRowFound -> Lwt.return_false

let already_follow_requested ~(src : Db.Account.t) ~(dst : Db.Account.t) :
    bool Lwt.t =
  match%lwt
    Db.(e @@ FollowRequest.get_one ~account_id:src#id ~target_account_id:dst#id)
  with
  | _ -> Lwt.return_true
  | exception Sqlx.Error.NoRowFound -> Lwt.return_false

let service ~(src : Db.Account.t) ~(dst : Db.Account.t) : unit Lwt.t =
  if%lwt follow_not_possible ~src ~dst then
    Httpq.Server.raise_error_response `Forbidden
  else
    if%lwt already_followed ~src ~dst then Lwt.return_unit
    else
      if%lwt already_follow_requested ~src ~dst then Lwt.return_unit
      else
        let uri = src#uri ^/ Uuidm.(v `V4 |> to_string) in
        match dst#domain with
        | None (* local *) -> direct_follow ~uri src dst
        | Some _ (* remote *) -> request_follow ~uri src dst

(* Recv POST /api/v1/accounts/:id/follow *)
let post req =
  let%lwt self = authenticate_account req in
  let acct_id = req |> Httpq.Server.param ":id" |> string_to_account_id in

  let%lwt acct = Db.e (Model.Account.get_one ~id:acct_id) in
  service ~src:self ~dst:acct;%lwt

  (* Return the result to the client *)
  let%lwt rel = make_relationship_from_model self acct in
  (* Pretend the follow succeeded *)
  let rel = { rel with following = true } in
  rel |> yojson_of_relationship |> respond_yojson
