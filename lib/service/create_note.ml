open Common
open Activity

(* Send Create/Note to POST /users/:name/inbox *)
let kick id (s : Db.Status.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt self = Db.Account.get ~by:(`id s.account_id) in
  let body =
    let published = s.created_at |> Ptime.to_rfc3339 in
    let to_ = [ "https://www.w3.org/ns/activitystreams#Public" ] in
    let cc = [ self.followers_url ] in
    let note =
      make_ap_note ~id:s.uri ~typ:"Note" ~published ~to_ ~cc
        ~attributedTo:self.uri ~content:s.text ()
    in
    make_ap_create ~context ~id:(s.uri ^/ "activity") ~typ:"Create"
      ~actor:(`String self.uri) ~published ~to_ ~cc ~obj:note ()
    |> ap_create_to_yojson
  in
  let%lwt dst = Db.Account.get ~by:(`id id) in
  post_activity_to_inbox ~body ~src:self ~dst
