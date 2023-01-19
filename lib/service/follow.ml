open Activity
open Util

(* Send Follow to POST inbox *)
let kick (self : Db.Account.t) (acc : Db.Account.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  (* NOTE: Assume there is no follow_request nor follow of (self_id, id) *)
  (* Insert follow_request *)
  let now = Ptime.now () in
  let uri = self.uri ^/ Uuidm.(v `V4 |> to_string) in
  Db.FollowRequest.make ~id:0 ~created_at:now ~updated_at:now
    ~account_id:self.id ~target_account_id:acc.id ~uri
  |> Db.FollowRequest.insert |> ignore_lwt;%lwt
  (* Post activity *)
  let body =
    make_ap_inbox ~context ~id:uri ~typ:"Follow" ~actor:(`String self.uri)
      ~obj:(`String acc.uri)
    |> ap_inbox_to_yojson
  in
  post_activity_to_inbox ~body ~src:self ~dst:acc
