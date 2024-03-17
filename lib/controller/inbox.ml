open Util
open Activity

(* Recv Follow in inbox *)
let kick_inbox_follow (req : ap_follow) =
  let src, dst =
    match (req.actor, req.obj) with
    | s, d when is_my_domain d -> (s, d)
    | _ -> Httpq.Server.raise_error_response `Bad_request
  in
  let%lwt src = search_account (`Uri src) in
  match%lwt Db.e (Model.Account.get_one ~uri:dst) with
  | exception Sqlx.Error.NoRowFound ->
      Httpq.Server.raise_error_response `Bad_request
  | dst ->
      Job.kick ~name:__FUNCTION__ @@ fun () ->
      let%lwt f =
        match%lwt
          Db.(e @@ Follow.get_one ~account_id:src#id ~target_account_id:dst#id)
        with
        | f -> Lwt.return f
        | exception Sqlx.Error.NoRowFound ->
            (* Insert to table 'follows' *)
            let now = Ptime.now () in
            let%lwt f =
              Db.(
                e
                @@ Follow.(
                     make ~created_at:now ~updated_at:now ~account_id:src#id
                       ~target_account_id:dst#id ~uri:req.id ()
                     |> save_one))
            in
            Worker.Local_notify.kick
              ~activity_id:(Model.Follow.ID.to_int f#id)
              ~activity_type:`Follow ~src ~dst ~typ:`follow;%lwt
            Lwt.return f
      in

      (* Send 'Accept' *)
      Worker.Accept.kick ~f ~followee:dst ~follower:src

(* Recv Accept in inbox *)
let kick_inbox_accept (req : ap_accept) =
  let uri =
    match req.obj with
    | Follow { id; _ } -> id
    | _ -> Httpq.Server.raise_error_response `Bad_request
  in
  match%lwt Db.(e @@ FollowRequest.get_one ~uri) with
  | exception Sqlx.Error.NoRowFound ->
      Httpq.Server.raise_error_response `Bad_request
  | r ->
      Job.kick ~name:__FUNCTION__ @@ fun () ->
      let now = Ptime.now () in
      Db.(e @@ FollowRequest.delete [ r ]) |> ignore_lwt;%lwt
      Db.(
        e
        @@ Follow.(
             make ~account_id:r#account_id
               ~target_account_id:r#target_account_id ~uri ~created_at:now
               ~updated_at:now ()
             |> save_one))
      |> ignore_lwt

let kick_inbox_undo_like (l : ap_like) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt fav = favourite_of_like ~must_already_exist:true l in
  Db.(e @@ Favourite.delete [ fav ])

let kick_inbox_undo_follow ({ id; _ } : ap_follow) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt follow = Db.(e @@ Follow.get_one ~uri:id) in
  Db.(e @@ Follow.delete [ follow ])

let kick_inbox_undo_announce (a : ap_announce) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt account = Db.e (Model.Account.get_one ~uri:a.actor) in
  let%lwt status = Db.(e Status.(get_one ~uri:a.id)) in
  Worker.Removal.kick ~account_id:account#id ~status_id:status#id

(* Recv Create in inbox *)
let kick_inbox_create (req : ap_create) =
  let note =
    match req with
    | { obj = Note note; _ } -> note
    | _ -> Httpq.Server.raise_error_response `Bad_request
  in
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt s = status_of_note note in
  Worker.Link_crawl.kick s#id;%lwt
  Worker.Distribute.kick s

(* Recv Announce in inbox *)
let kick_inbox_announce (req : ap_announce) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt s = status_of_announce req in
  Worker.Distribute.kick s;%lwt
  let%lwt s' = Db.e (Model.Status.get_one ~id:(Option.get s#reblog_of_id)) in
  let%lwt dst = Db.e (Model.Account.get_one ~id:s'#account_id) in
  if Model.Account.is_local dst then
    let%lwt src = Db.e (Model.Account.get_one ~id:s#account_id) in
    Worker.Local_notify.kick
      ~activity_id:(Model.Status.ID.to_int s#id)
      ~activity_type:`Status ~typ:`reblog ~src ~dst
  else Lwt.return_unit

(* Recv Like in inbox *)
let kick_inbox_like (req : ap_like) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt f = favourite_of_like req in
  let%lwt src = Db.e (Model.Account.get_one ~id:f#account_id) in
  let%lwt s = Db.e (Model.Status.get_one ~id:f#status_id) in
  let%lwt dst = Db.e (Model.Account.get_one ~id:s#account_id) in
  Worker.Local_notify.kick
    ~activity_id:(Model.Favourite.ID.to_int f#id)
    ~activity_type:`Favourite ~typ:`favourite ~src ~dst;%lwt
  Lwt.return_unit

let kick_inbox_delete (req : ap_delete) =
  match%lwt Db.e (Model.Account.get_one ~uri:req.actor) with
  | exception Sqlx.Error.NoRowFound ->
      (* Unknown account; just ignore *)
      Lwt.return_unit
  | account -> (
      (* FIXME: Support deletion of accounts *)
      match req.obj |> of_yojson |> get_tombstone with
      | (exception _) | None ->
          (* Unknown object; just ignore *) Lwt.return_unit
      | Some tomb -> (
          match%lwt Db.(e Status.(get_many ~uri:tomb.id)) with
          | [] -> Lwt.return_unit
          | [ status ] ->
              Worker.Removal.kick ~account_id:account#id ~status_id:status#id
          | _ :: _ -> failwith "Internal error: not unique status uri"))

let kick_inbox_update_person (r : ap_person) =
  let%lwt a = Db.e (Model.Account.get_one ~uri:r.id) in
  let a = Activity.model_account_of_person ~original:a r in
  Db.(e @@ Account.save_one a) |> ignore_lwt

(* Recv POST /users/:name/inbox *)
let post req =
  match%lwt Activity.verify_activity_json req with
  | body, Error (`AccountNotFound | `AccountIsLocal) ->
      Logq.err (fun m -> m "account not found or account is local: %s" body);
      Httpq.Server.respond ~status:`Accepted ""
  | body, Error (`VerifFailure e) ->
      Logq.err (fun m ->
          m "verification failure of activity json: %s: %s"
            (match e with
            | `AlgorithmNotImplemented -> "algorithm not implemented"
            | `Msg msg -> msg)
            body);
      Httpq.Server.respond ~status:`Unauthorized ""
  | body, Ok () -> (
      try%lwt
        (*Logq.debug (fun m -> m ">>>>>>>>\n%s" body);*)
        (match Yojson.Safe.from_string body |> of_yojson with
        | Accept r -> kick_inbox_accept r
        | Announce r -> kick_inbox_announce r
        | Create r -> kick_inbox_create r
        | Delete r -> kick_inbox_delete r
        | Follow r -> kick_inbox_follow r
        | Like r -> kick_inbox_like r
        | Undo { obj = Follow v; _ } -> kick_inbox_undo_follow v
        | Undo { obj = Like v; _ } -> kick_inbox_undo_like v
        | Undo { obj = Announce v; _ } -> kick_inbox_undo_announce v
        | Update { obj = Person r; _ } -> kick_inbox_update_person r
        | _ -> failwith "activity not implemented");%lwt
        Httpq.Server.respond ~status:`Accepted ""
      with e ->
        Logq.err (fun m ->
            m
              "Failed to handle inboxed message; the response and request will \
               be printed in log:\n\
               %s\n\
               %s"
              (Printexc.to_string e)
              (Printexc.get_backtrace ()));
        Httpq.Server.respond ~status:`Accepted ~tags:[ "log" ] "")
