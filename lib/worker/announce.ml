open Activity

(* Send Announce to POST /users/:name/inbox *)
let kick env ~(url : string) ~(status : Db.Status.t) =
  let activity = announce_of_status status |> announce in
  let src = Db.e (Model.Account.get_one ~id:status#account_id) in
  Delivery.kick env ~activity ~src ~url
