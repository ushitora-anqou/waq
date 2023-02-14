open Activity
open Lwt.Infix

(* Send Announce to POST /users/:name/inbox *)
let kick (dst : Db.Account.t) (s : Db.Status.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt body = announce_of_status s >|= announce >|= to_yojson in
  let%lwt src = Db.Account.get_one ~id:s.account_id () in
  post_activity_to_inbox ~body ~src ~dst
