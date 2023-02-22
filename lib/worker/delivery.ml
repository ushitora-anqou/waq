open Activity

let kick ~(activity : t) ~(url : string) ~(src : Db.Account.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let body = to_yojson activity in
  let sign, body = sign_activity ~body ~src in
  post_activity_json ~body ~sign ~url
