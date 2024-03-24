open Activity

(* Send Accept to POST inbox *)
let kick env ~(f : Db.Follow.t) ~(followee : Db.Account.t)
    ~(follower : Db.Account.t) =
  let id =
    followee#uri ^ "#accepts/follows/"
    ^ (f#id |> Model.Follow.ID.to_int |> string_of_int)
  in
  let obj =
    make_follow ~id:f#uri ~actor:follower#uri ~obj:followee#uri |> follow
  in
  let activity = make_accept ~id ~actor:(`String followee#uri) ~obj |> accept in
  Delivery.kick env ~activity ~src:followee ~url:follower#inbox_url
