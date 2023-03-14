open Util
open Lwt.Infix

let kick ~(activity_id : int) ~(activity_type : Db.Notification.activity_type_t)
    ~(dst : Db.Account.t) ~(src : Db.Account.t) ~(typ : Db.Notification.typ_t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let account_id = dst#id in
  let from_account_id = src#id in
  let typ = Some typ in
  let now = Ptime.now () in
  match%lwt
    Db.e
    @@ Model.Notification.get_many ~activity_id ~activity_type ~account_id
         ~from_account_id ~typ
  with
  | _ :: _ -> Lwt.return_unit
  | [] ->
      let%lwt u = Db.e @@ Model.User.get_one ~account_id in
      let%lwt n =
        Db.e
        @@ Model.Notification.(
             make ~activity_id ~activity_type ~created_at:now ~updated_at:now
               ~account_id ~from_account_id ?typ ()
             |> save_one)
      in
      let%lwt payload =
        Entity.make_notification_from_model ~self_id:account_id n
        >|= Entity.yojson_of_notification >|= Yojson.Safe.to_string
      in
      Streaming.(
        push
          ~key:(make_key ~user_id:u#id ~stream:`User)
          ~event:"notification" ~payload ());
      Lwt.return_unit
