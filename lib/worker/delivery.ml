let kick ~(activity : Activity.t) ~(dst : Db.Account.t) ~(src : Db.Account.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let body = Activity.to_yojson activity in
  Activity.post_activity_to_inbox ~body ~src ~dst
