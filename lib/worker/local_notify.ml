open Util

let service env activity_id activity_type dst src typ () =
  let account_id = dst#id in
  let from_account_id = src#id in
  let typ = Some typ in
  let now = Ptime.now () in
  match
    Db.e
    @@ Model.Notification.get_many ~activity_id ~activity_type ~account_id
         ~from_account_id ~typ
  with
  | _ :: _ -> ()
  | [] ->
      let u = Db.e @@ Model.User.get_one ~account_id in
      let n =
        Db.e
        @@ Model.Notification.(
             make ~activity_id ~activity_type ~created_at:now ~updated_at:now
               ~account_id ~from_account_id ?typ ()
             |> save_one ~preload:[ `target_status []; `from_account [] ])
      in

      (* Notification via WebSocket *)
      ( Entity.load_notifications_from_db ~self_id:account_id [ n#id ]
      |> List.hd |> Entity.yojson_of_notification |> Yojson.Safe.to_string
      |> fun payload ->
        Streaming.(
          push
            ~key:(make_key ~user_id:u#id ~stream:`User)
            ~event:"notification" ~payload ()) );

      (* Push notification *)
      Webpush_helper.deliver env ~user_id:u#id
        (Entity.serialize_push_notification n
        |> Entity.yojson_of_push_notification |> Yojson.Safe.to_string)

let kick env ~(activity_id : int)
    ~(activity_type : Db.Notification.activity_type_t) ~(dst : Db.Account.t)
    ~(src : Db.Account.t) ~(typ : Db.Notification.typ_t) =
  if not (Model.Account.is_local dst) then
    Logq.err (fun m ->
        m "Local_notify.kick: dst is not a local account: %s"
          (Model.Notification.activity_type_t_to_string activity_type))
  else
    Job.kick env ~name:__FUNCTION__
      (service env activity_id activity_type dst src typ)
