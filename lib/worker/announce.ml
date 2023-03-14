open Activity
open Lwt.Infix

(* Send Announce to POST /users/:name/inbox *)
let kick ~(url : string) ~(status : Db.Status.t) =
  let%lwt activity = announce_of_status status >|= announce in
  let%lwt src = Db.e (Model.Account.get_one ~id:status#account_id) in
  Delivery.kick ~activity ~src ~url
