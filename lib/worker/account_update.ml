open Util
open Lwt.Infix

let kick ~account_id ~updated_at =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt src = Db.Account.get_one ~id:account_id () in
  let id = src.uri ^ "#updates" ^/ (Ptime.to_int updated_at |> string_of_int) in
  let actor = src.uri in
  let to_ = [ "https://www.w3.org/ns/activitystreams#Public" ] in
  let obj = Activity.(person_of_account src |> person) in
  let activity = Activity.(make_update ~id ~actor ~to_ ~obj |> update) in
  Db.get_remote_followers ~account_id
  >|= Db.Account.preferred_inbox_urls
  >>= Lwt_list.iter_p (fun url -> Delivery.kick ~src ~url ~activity)
