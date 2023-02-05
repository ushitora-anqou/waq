open Activity

(* Send Accept to POST inbox *)
let kick ~(f : Db.Follow.t) ~(followee : Db.Account.t)
    ~(follower : Db.Account.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let id = followee.uri ^ "#accepts/follows/" ^ string_of_int f.id in
  let obj =
    make_follow ~id:f.uri ~actor:follower.uri ~obj:followee.uri |> of_follow
  in
  let body =
    make_accept ~id ~actor:(`String followee.uri) ~obj |> of_accept |> to_yojson
  in
  post_activity_to_inbox ~body ~src:followee ~dst:follower
