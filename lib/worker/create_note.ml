open Activity

(* Send Create/Note to POST /users/:name/inbox *)
let kick env ~(url : string) ~(status : Db.Status.t) =
  let activity = create_note_of_status status |> create in
  let src = Db.e (Model.Account.get_one ~id:status#account_id) in
  Delivery.kick env ~activity ~src ~url
