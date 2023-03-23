open Activity

let kick ~(activity : t) ~(url : string) ~(src : Db.Account.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let body = activity |> to_yojson |> Yojson.Safe.to_string in
  post_activity_json ~body ~sign:(sign_spec_of_account src) ~url
