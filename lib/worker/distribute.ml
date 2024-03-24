open Util

let deliver_to_local env ~(targets : Db.User.t list) ~(status : Db.Status.t) :
    unit =
  targets
  |> List.iter (fun (u : Db.User.t) ->
         Insert_to_feed.kick env ~account_id:u#account_id ~status_id:status#id
           ~user_id:u#id ~stream:`User)

let deliver_to_remote env ~(targets : Db.Account.t list) ~(status : Db.Status.t)
    : unit =
  targets |> Db.Account.preferred_inbox_urls
  |> List.iter (fun url ->
         match status#reblog_of_id with
         | None -> Create_note.kick env ~status ~url
         | Some _ -> Announce.kick env ~status ~url)

let kick env (s : Db.Status.t) =
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let a = Db.e (Model.Account.get_one ~id:s#account_id) in
  let is_status_from_remote = a#domain <> None in

  let local_followers = Db.(e @@ get_local_followers ~account_id:a#id) in
  let remote_followers = Db.(e @@ get_remote_followers ~account_id:a#id) in

  let local_mentions, remote_mentions =
    Db.(
      e
      @@ Mention.select ~status_id:(`Eq s#id) ~account_id:`NeqNone
           ~preload:[ `account [ `user [] ]; `status [ `account [] ] ])
    |> List.partition_map (fun x ->
           let acct = Option.get x#account in
           match acct#user with None -> Right x | Some _ -> Left x)
  in

  (* Deliver to self *)
  (if is_status_from_remote then () (* Just ignore *)
   else
     let u = Db.(e @@ User.get_one ~account_id:a#id) in
     (* Local: Send the status to self *)
     Insert_to_feed.kick env ~account_id:a#id ~status_id:s#id ~user_id:u#id
       ~stream:`User);

  (* Deliver to remote followers *)
  (if not is_status_from_remote then
     let targets = remote_followers in
     let targets =
       targets @ (remote_mentions |> List.map (fun x -> Option.get x#account))
     in
     let targets =
       match s#reblog_of_id with
       | None -> targets
       | Some id ->
           (* Deliver Announce activity to the original author *)
           let s = Db.(e Status.(get_one ~id ~preload:[ `account [] ])) in
           if Model.Account.is_remote s#account then s#account :: targets
           else targets
     in
     deliver_to_remote env ~targets ~status:s);

  (* Deliver to local followers *)
  deliver_to_local env
    ~targets:
      (local_followers
      @ (local_mentions
        |> List.map (fun x -> Option.get (Option.get x#account)#user)))
    ~status:s;

  (* Send notification to mentioned local users *)
  (local_mentions
  |> List.iter @@ fun m ->
     let src = (Option.get m#status)#account in
     let dst = Option.get m#account in
     Local_notify.kick env
       ~activity_id:(Model.Mention.ID.to_int m#id)
       ~activity_type:`Mention ~typ:`mention ~src ~dst);

  ()
