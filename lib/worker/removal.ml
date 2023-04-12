open Lwt.Infix

let aux ~account_id ~status_id ~status =
  let%lwt reblogs = Db.(e @@ Status.get_many' ~reblog_of_id:(Some status_id)) in
  let%lwt src = Db.e (Model.Account.get_one ~id:account_id) in
  let is_account_local = Model.Account.is_local src |> Lwt.return in

  let deliver_to_local ~user_id =
    let open Streaming in
    let key = make_key ~user_id ~stream:`User in
    status :: reblogs
    |> List.iter (fun (status : Db.Status.t) ->
           let payload = status#id |> Model.Status.ID.to_int |> string_of_int in
           push ~key ~event:"delete" ~payload ())
  in

  (* Deliver to self if necessary *)
  (if%lwt is_account_local then
     Db.(e @@ User.get_one ~account_id) >|= fun u ->
     deliver_to_local ~user_id:u#id);%lwt

  (* Deliver to local followers *)
  Db.(e @@ get_local_followers ~account_id)
  >|= List.iter (fun (user : Db.User.t) -> deliver_to_local ~user_id:user#id);%lwt

  (* Deliver to remote followers *)
  if%lwt is_account_local then (
    let%lwt activity =
      let open Activity in
      match status#reblog_of_id with
      | Some _ ->
          (* Undo Announce *)
          announce_of_status ~deleted:true status
          >|= announce >|= to_undo ~actor:src#uri >|= undo
      | None ->
          (* Delete *)
          let id = status#uri ^ "#delete" in
          let actor = src#uri in
          let to_ = [ "https://www.w3.org/ns/activitystreams#Public" ] in
          let obj = make_tombstone ~id:status#uri |> tombstone |> to_yojson in
          make_delete ~id ~actor ~to_ ~obj |> delete |> Lwt.return
    in
    Db.(e @@ get_remote_followers ~account_id)
    >|= Db.Account.preferred_inbox_urls
    >>= Lwt_list.iter_p (fun url -> Delivery.kick ~src ~url ~activity);%lwt

    Lwt.return_unit)

let kick ~account_id ~status_id =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt status = Db.(e @@ Status.discard_with_reblogs status_id) in
  if account_id <> status#account_id then (
    Logq.err (fun m ->
        m "Worker.Removal: ignoring invalid pair of account_id and status_id");
    Lwt.return_unit)
  else aux ~account_id ~status_id ~status
