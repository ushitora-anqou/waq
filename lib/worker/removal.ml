open Lwt.Infix

let kick ~self_id ~status_id =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt status = Db.Status.discard_with_reblogs status_id in
  let%lwt reblogs = Db.Status.get_many' ~reblog_of_id:(Some status_id) () in

  let deliver_to_local ~user_id =
    let open Streaming in
    let key = make_key ~user_id ~stream:`User in
    status :: reblogs
    |> List.iter (fun (status : Db.Status.t) ->
           let payload = string_of_int status.id in
           push ~key ~event:"delete" ~payload ())
  in

  (* Deliver to self *)
  let%lwt u = Db.User.get_one ~account_id:self_id () in
  deliver_to_local ~user_id:u.id;

  (* Deliver to local followers *)
  Db.get_local_followers ~account_id:self_id
  >|= List.iter (fun (user : Db.User.t) -> deliver_to_local ~user_id:user.id);%lwt

  (* FIXME: Deliver to remote followers *)
  Lwt.return_unit
