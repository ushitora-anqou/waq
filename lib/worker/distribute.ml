open Util

let deliver_to_local ~(followers : Db.User.t list) ~(status : Db.Status.t) :
    unit Lwt.t =
  followers
  |> Lwt_list.iter_p (fun (u : Db.User.t) ->
         Insert_to_feed.kick ~account_id:u#account_id ~status_id:status#id
           ~user_id:u#id ~stream:`User)

let deliver_to_remote ~(followers : Db.Account.t list) ~(status : Db.Status.t) :
    unit Lwt.t =
  followers |> Db.Account.preferred_inbox_urls
  |> Lwt_list.iter_p (fun url ->
         match status#reblog_of_id with
         | None -> Create_note.kick ~status ~url
         | Some _ -> Announce.kick ~status ~url)

let kick (s : Db.Status.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt a = Db.e (Model.Account.get_one ~id:s#account_id) in
  let is_status_from_remote = a#domain <> None in

  let%lwt local_followers = Db.(e @@ get_local_followers ~account_id:a#id) in
  let%lwt remote_followers = Db.(e @@ get_remote_followers ~account_id:a#id) in

  (* Deliver to self *)
  (if is_status_from_remote then Lwt.return_unit (* Just ignore *)
   else
     let%lwt u = Db.(e @@ User.get_one ~account_id:a#id) in
     (* Local: Send the status to self *)
     Insert_to_feed.kick ~account_id:a#id ~status_id:s#id ~user_id:u#id
       ~stream:`User);%lwt

  (* Deliver to remote followers *)
  if is_status_from_remote && remote_followers <> [] then
    Logq.warn (fun m ->
        m
          "Found a follow from a remote user to another remote one; possibly a \
           bug");
  deliver_to_remote ~followers:remote_followers ~status:s;%lwt

  (* Deliver to local followers *)
  deliver_to_local ~followers:local_followers ~status:s;%lwt

  Lwt.return_unit
