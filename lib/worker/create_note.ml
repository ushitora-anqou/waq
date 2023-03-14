open Activity
open Lwt.Infix

(* Send Create/Note to POST /users/:name/inbox *)
let kick ~(url : string) ~(status : Db.Status.t) =
  let%lwt activity = create_note_of_status status >|= create in
  let%lwt src = Db.e (Model.Account.get_one ~id:status#account_id) in
  Delivery.kick ~activity ~src ~url
