open Lwt.Infix
open Util

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
    Insert_to_feed.kick ~status_id:s.id ~user_id:u.id ~stream:`User;
    Lwt.return_unit);%lwt

  (* Deliver to remote followers *)
  if is_status_from_remote && remote_followers <> [] then
    Logq.warn (fun m ->
        m
          "Found a follow from a remote user to another remote one; possibly a \
           bug");
  remote_followers
  |> List.iter (fun (a : Db.Account.t) ->
         Create_note.kick a.id s (* FIXME: use sharedInbox *));

  (* Deliver to local followers *)
  local_followers
  |> List.iter (fun (u : Db.User.t) ->
         Insert_to_feed.kick ~status_id:s.id ~user_id:u.id ~stream:`User);

  Lwt.return_unit
