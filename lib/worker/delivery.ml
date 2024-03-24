open Activity

let kick env ~(activity : t) ~(url : string) ~(src : Db.Account.t) =
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let body = activity |> to_yojson |> Yojson.Safe.to_string in
  post_activity_json env ~body ~sign:(sign_spec_of_account src) ~url
