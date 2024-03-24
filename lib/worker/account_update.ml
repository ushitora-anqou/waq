open Util

let kick env ~account_id ~updated_at =
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let src = Db.e (Model.Account.get_one ~id:account_id) in
  let id = src#uri ^ "#updates" ^/ (Ptime.to_int updated_at |> string_of_int) in
  let actor = src#uri in
  let to_ = [ "https://www.w3.org/ns/activitystreams#Public" ] in
  let obj = Activity.(person_of_account src |> person) in
  let activity = Activity.(make_update ~id ~actor ~to_ ~obj |> update) in
  Db.(e @@ get_remote_followers ~account_id)
  |> Db.Account.preferred_inbox_urls
  |> List.iter (fun url -> Delivery.kick env ~src ~url ~activity)
