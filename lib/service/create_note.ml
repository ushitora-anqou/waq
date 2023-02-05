open Activity
open Lwt.Infix

(* Send Create/Note to POST /users/:name/inbox *)
let kick id (s : Db.Status.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt body = create_note_of_status s >|= of_create >|= to_yojson in
  let%lwt dst = Db.Account.get_one ~id () in
  let%lwt src = Db.Account.get_one ~id:s.account_id () in
  post_activity_to_inbox ~body ~src ~dst
