open Lwt.Infix

let kick (s : Db.Status.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt a = Db.Account.get_one ~id:s.account_id () in
  let is_status_from_remote = a.domain <> None in
  let deliver_to_self =
    if is_status_from_remote then Lwt.return_unit (* Just ignore *)
    else
      let%lwt u = Db.User.get_one ~account_id:a.id () in
      (* Local: Send the status to self *)
      Insert_to_feed.kick ~status_id:s.id ~user_id:u.id ~stream:`User;
      Lwt.return_unit
  in
  let deliver_to_followers =
    let%lwt followers = Db.Follow.get_many ~target_account_id:a.id () in
    followers
    |> Lwt_list.iter_p @@ fun (f : Db.Follow.t) ->
       let%lwt a = Db.Account.get_one ~id:f.account_id () in
       match a.domain with
       | None ->
           (* To local *)
           Db.User.get_one ~account_id:a.id () >|= fun u ->
           Insert_to_feed.kick ~status_id:s.id ~user_id:u.id ~stream:`User
       | Some _ ->
           (* To remote *)
           Lwt.return
           @@
           if is_status_from_remote then
             Log.warn (fun m ->
                 m
                   "Found a follow from a remote user to another remote one; \
                    possibly a bug: %d %d"
                   f.account_id f.target_account_id)
           else (* FIXME: Use shared inbox *)
             Create_note.kick f.account_id s
  in
  Lwt.join [ deliver_to_self; deliver_to_followers ]
