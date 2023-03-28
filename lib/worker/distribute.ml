open Util
open Lwt.Infix

let deliver_to_local ~(targets : Db.User.t list) ~(status : Db.Status.t) :
    unit Lwt.t =
  targets
  |> Lwt_list.iter_p (fun (u : Db.User.t) ->
         Insert_to_feed.kick ~account_id:u#account_id ~status_id:status#id
           ~user_id:u#id ~stream:`User)

let deliver_to_remote ~(targets : Db.Account.t list) ~(status : Db.Status.t) :
    unit Lwt.t =
  targets |> Db.Account.preferred_inbox_urls
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

  let%lwt local_mentions, remote_mentions =
    Db.(
      e
      @@ Mention.select ~status_id:(`Eq s#id) ~account_id:`NeqNone
           ~preload:[ `account [ `user [] ]; `status [ `account [] ] ])
    >|= List.partition_map (fun x ->
            let acct = Option.get x#account in
            match acct#user with None -> Right x | Some _ -> Left x)
  in

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
  deliver_to_remote
    ~targets:
      (remote_followers
      @ (remote_mentions |> List.map (fun x -> Option.get x#account)))
    ~status:s;%lwt

  (* Deliver to local followers *)
  deliver_to_local
    ~targets:
      (local_followers
      @ (local_mentions
        |> List.map (fun x -> Option.get (Option.get x#account)#user)))
    ~status:s;%lwt

  (* Send notification to mentioned local users *)
  (local_mentions
  |> Lwt_list.iter_p @@ fun m ->
     let src = (Option.get m#status)#account in
     let dst = Option.get m#account in
     Local_notify.kick
       ~activity_id:(Model.Mention.ID.to_int m#id)
       ~activity_type:`Mention ~typ:`mention ~src ~dst);%lwt

  Lwt.return_unit
