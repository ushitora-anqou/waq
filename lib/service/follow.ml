open Activity
open Util

let request_follow ~now ~uri (self : Db.Account.t) (acc : Db.Account.t) =
  (* FIXME: Assume acc is a remote account *)
  assert (acc.domain <> None);

  (* Insert follow_request *)
  Db.FollowRequest.make ~id:0 ~created_at:now ~updated_at:now
    ~account_id:self.id ~target_account_id:acc.id ~uri
  |> Db.FollowRequest.save_one |> ignore_lwt;%lwt

  (* Post activity *)
  let body =
    make_follow ~id:uri ~actor:self.uri ~obj:acc.uri |> of_follow |> to_yojson
  in
  post_activity_to_inbox ~body ~src:self ~dst:acc

let direct_follow ~now ~uri (self : Db.Account.t) (acc : Db.Account.t) =
  (* Assume acc is a local account *)
  assert (acc.domain = None);

  (* Insert follow *)
  Db.Follow.(
    make ~id:0 ~created_at:now ~updated_at:now ~account_id:self.id
      ~target_account_id:acc.id ~uri
    |> save_one)
  |> ignore_lwt

(* Send Follow to POST inbox *)
let kick (self : Db.Account.t) (acc : Db.Account.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  (* NOTE: Assume there is no follow_request nor follow of (self_id, id) *)
  let now = Ptime.now () in
  let uri = self.uri ^/ Uuidm.(v `V4 |> to_string) in
  match acc.domain with
  | None (* local *) -> direct_follow ~now ~uri self acc
  | Some _ (* remote *) -> request_follow ~now ~uri self acc
