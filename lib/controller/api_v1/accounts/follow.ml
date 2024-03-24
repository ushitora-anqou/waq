open Entity
open Helper
open Util

let request_follow env ~uri (self : Db.Account.t) (acc : Db.Account.t) =
  (* FIXME: Assume acc is a remote account *)
  assert (acc#domain <> None);

  (* Insert follow_request *)
  Db.(
    e
    @@ FollowRequest.(
         make ~account_id:self#id ~target_account_id:acc#id ~uri () |> save_one))
  |> ignore;

  (* Post activity *)
  let open Activity in
  let activity = make_follow ~id:uri ~actor:self#uri ~obj:acc#uri |> follow in
  Worker.Delivery.kick env ~activity ~src:self ~url:acc#inbox_url

let direct_follow env ~uri (self : Db.Account.t) (acc : Db.Account.t) =
  (* Assume acc is a local account *)
  assert (acc#domain = None);

  (* Insert follow *)
  let f =
    Db.(
      e
      @@ Follow.(
           make ~account_id:self#id ~target_account_id:acc#id ~uri ()
           |> save_one))
  in

  (* Notify *)
  Worker.Local_notify.kick env
    ~activity_id:(Model.Follow.ID.to_int f#id)
    ~activity_type:`Follow ~dst:acc ~src:self ~typ:`follow

let follow_not_possible ~(src : Db.Account.t) ~(dst : Db.Account.t) : bool =
  src#id = dst#id

let already_followed ~(src : Db.Account.t) ~(dst : Db.Account.t) : bool =
  match
    Db.(e @@ Follow.get_one ~account_id:src#id ~target_account_id:dst#id)
  with
  | _ -> true
  | exception Sqlx.Error.NoRowFound -> false

let already_follow_requested ~(src : Db.Account.t) ~(dst : Db.Account.t) : bool
    =
  match
    Db.(e @@ FollowRequest.get_one ~account_id:src#id ~target_account_id:dst#id)
  with
  | _ -> true
  | exception Sqlx.Error.NoRowFound -> false

let service env ~(src : Db.Account.t) ~(dst : Db.Account.t) : unit =
  if follow_not_possible ~src ~dst then
    Yume.Server.raise_error_response `Forbidden
  else if already_followed ~src ~dst then ()
  else if already_follow_requested ~src ~dst then ()
  else
    let uri = src#uri ^/ Uuidm.(v `V4 |> to_string) in
    match dst#domain with
    | None (* local *) -> direct_follow env ~uri src dst
    | Some _ (* remote *) -> request_follow env ~uri src dst

(* Recv POST /api/v1/accounts/:id/follow *)
let post env req =
  let self = authenticate_account req in
  let acct_id = req |> Yume.Server.param ":id" |> string_to_account_id in

  let acct = Db.e (Model.Account.get_one ~id:acct_id) in
  service env ~src:self ~dst:acct;

  (* Return the result to the client *)
  let rel = make_relationship_from_model self acct in
  (* Pretend the follow succeeded *)
  let rel = { rel with following = true } in
  rel |> yojson_of_relationship |> respond_yojson
