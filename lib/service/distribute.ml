open Lwt.Infix
open Util

let deliver_to_local ~(followers : Db.User.t list) ~(status : Db.Status.t) :
    unit =
  followers
  |> List.iter (fun (u : Db.User.t) ->
         Insert_to_feed.kick ~account_id:u.account_id ~status_id:status.id
           ~user_id:u.id ~stream:`User)

let deliver_to_remote ~(followers : Db.Account.t list) ~(status : Db.Status.t) :
    unit =
  (* FIXME: use sharedInbox *)
  followers
  |> List.iter (fun (a : Db.Account.t) ->
         match status.reblog_of_id with
         | None -> Create_note.kick a.id status
         | Some _ -> Announce.kick a status)

let kick (s : Db.Status.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt a = Db.Account.get_one ~id:s.account_id () in
  let is_status_from_remote = a.domain <> None in

  let%lwt followers = Db.Follow.get_many ~target_account_id:a.id () in
  let%lwt local_followers, remote_followers =
    followers
    |> Lwt_list.partition_map_p (fun (f : Db.Follow.t) ->
           let%lwt a = Db.Account.get_one ~id:f.account_id () in
           match a.domain with
           | None (* local *) ->
               Db.User.get_one ~account_id:a.id () >|= fun u -> Either.Left u
           | Some _ (* remote *) -> Lwt.return (Either.Right a))
  in

  (* Deliver to self *)
  (if is_status_from_remote then Lwt.return_unit (* Just ignore *)
  else
    let%lwt u = Db.User.get_one ~account_id:a.id () in
    (* Local: Send the status to self *)
    Insert_to_feed.kick ~account_id:a.id ~status_id:s.id ~user_id:u.id
      ~stream:`User;
    Lwt.return_unit);%lwt

  (* Deliver to remote followers *)
  if is_status_from_remote && remote_followers <> [] then
    Logq.warn (fun m ->
        m
          "Found a follow from a remote user to another remote one; possibly a \
           bug");
  deliver_to_remote ~followers:remote_followers ~status:s;

  (* Deliver to local followers *)
  deliver_to_local ~followers:local_followers ~status:s;

  Lwt.return_unit
