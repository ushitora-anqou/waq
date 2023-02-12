open Util

let kick ~(activity_id : int) ~(activity_type : string) ~(dst : Db.Account.t)
    ~(src : Db.Account.t) ~(typ : string) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let open Db.Notification in
  let account_id = dst.id in
  let from_account_id = src.id in
  let typ = Some typ in
  let now = Ptime.now () in
  match%lwt
    get_many ~activity_id ~activity_type ~account_id ~from_account_id ~typ ()
  with
  | _ :: _ -> Lwt.return_unit
  | [] ->
      make ~id:0 ~activity_id ~activity_type ~created_at:now ~updated_at:now
        ~account_id ~from_account_id ?typ ()
      |> save_one |> ignore_lwt
