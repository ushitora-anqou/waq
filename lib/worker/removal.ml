open Lwt.Infix

let kick ~account_id ~status_id =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt status = Db.Status.discard_with_reblogs status_id in
  let%lwt reblogs = Db.Status.get_many' ~reblog_of_id:(Some status_id) () in
  let%lwt src = Db.Account.get_one ~id:account_id () in
  let is_account_local = Db.Account.is_local ~id:account_id in

  let deliver_to_local ~user_id =
    let open Streaming in
    let key = make_key ~user_id ~stream:`User in
    status :: reblogs
    |> List.iter (fun (status : Db.Status.t) ->
           let payload = string_of_int status.id in
           push ~key ~event:"delete" ~payload ())
  in
  let delivery_to_remote ~acct =
    let open Activity in
    status :: reblogs
    |> Lwt_list.iter_p (fun (status : Db.Status.t) ->
           let id = status.uri ^ "#delete" in
           let actor = src.uri in
           let to_ = [ "https://www.w3.org/ns/activitystreams#Public" ] in
           let obj = make_tombstone ~id:status.uri |> tombstone in
           let activity = make_delete ~id ~actor ~to_ ~obj |> delete in
           Delivery.kick ~src ~dst:acct ~activity)
  in

  (* Deliver to self if necessary *)
  (if%lwt is_account_local then
   Db.User.get_one ~account_id () >|= fun u -> deliver_to_local ~user_id:u.id);%lwt

  (* Deliver to local followers *)
  Db.get_local_followers ~account_id
  >|= List.iter (fun (user : Db.User.t) -> deliver_to_local ~user_id:user.id);%lwt

  (* Deliver to remote followers *)
  if%lwt is_account_local then
    Db.get_remote_followers ~account_id
    >>= Lwt_list.iter_p (fun (acct : Db.Account.t) -> delivery_to_remote ~acct);%lwt

  Lwt.return_unit
