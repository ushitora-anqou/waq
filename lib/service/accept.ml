open Common
open Activity

(* Send Accept to POST inbox *)
let kick ~(f : Db.Follow.t) ~(followee : Db.Account.t)
    ~(follower : Db.Account.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let id = followee.uri ^ "#accepts/follows/" ^ string_of_int f.id in
  let obj =
    make_ap_inbox_no_context ~id:f.uri ~typ:"Follow"
      ~actor:(`String follower.uri) ~obj:(`String followee.uri)
    |> ap_inbox_no_context_to_yojson
  in
  let body =
    make_ap_inbox ~context ~id ~typ:"Accept" ~actor:(`String followee.uri) ~obj
    |> ap_inbox_to_yojson
  in
  post_activity_to_inbox ~body ~src:followee ~dst:follower
