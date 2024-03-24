open Util
open Activity

(* Recv Follow in inbox *)
let kick_inbox_follow env (req : ap_follow) =
  let src, dst =
    match (req.actor, req.obj) with
    | s, d when is_my_domain d -> (s, d)
    | _ -> Yume.Server.raise_error_response `Bad_request
  in
  let src = search_account env (`Uri src) in
  let dst =
    try Db.e (Model.Account.get_one ~uri:dst)
    with Sqlx.Error.NoRowFound ->
      Yume.Server.raise_error_response `Bad_request
  in
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let f =
    try Db.(e @@ Follow.get_one ~account_id:src#id ~target_account_id:dst#id)
    with Sqlx.Error.NoRowFound ->
      (* Insert to table 'follows' *)
      let now = Ptime.now () in
      let f =
        Db.(
          e
          @@ Follow.(
               make ~created_at:now ~updated_at:now ~account_id:src#id
                 ~target_account_id:dst#id ~uri:req.id ()
               |> save_one))
      in
      Worker.Local_notify.kick env
        ~activity_id:(Model.Follow.ID.to_int f#id)
        ~activity_type:`Follow ~src ~dst ~typ:`follow;
      f
  in

  (* Send 'Accept' *)
  Worker.Accept.kick env ~f ~followee:dst ~follower:src

(* Recv Accept in inbox *)
let kick_inbox_accept env (req : ap_accept) =
  let uri =
    match req.obj with
    | Follow { id; _ } -> id
    | _ -> Yume.Server.raise_error_response `Bad_request
  in
  let r =
    try Db.(e @@ FollowRequest.get_one ~uri)
    with Sqlx.Error.NoRowFound ->
      Yume.Server.raise_error_response `Bad_request
  in
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let now = Ptime.now () in
  Db.(e @@ FollowRequest.delete [ r ]) |> ignore;
  Db.(
    e
    @@ Follow.(
         make ~account_id:r#account_id ~target_account_id:r#target_account_id
           ~uri ~created_at:now ~updated_at:now ()
         |> save_one))
  |> ignore

let kick_inbox_undo_like env (l : ap_like) =
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let fav = favourite_of_like ~must_already_exist:true l in
  Db.(e @@ Favourite.delete [ fav ])

let kick_inbox_undo_follow env ({ id; _ } : ap_follow) =
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let follow = Db.(e @@ Follow.get_one ~uri:id) in
  Db.(e @@ Follow.delete [ follow ])

let kick_inbox_undo_announce env (a : ap_announce) =
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let account = Db.e (Model.Account.get_one ~uri:a.actor) in
  let status = Db.(e Status.(get_one ~uri:a.id)) in
  Worker.Removal.kick env ~account_id:account#id ~status_id:status#id

(* Recv Create in inbox *)
let kick_inbox_create env (req : ap_create) =
  let note =
    match req with
    | { obj = Note note; _ } -> note
    | _ -> Yume.Server.raise_error_response `Bad_request
  in
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let s = status_of_note env note in
  Worker.Link_crawl.kick env s#id;
  Worker.Distribute.kick env s

(* Recv Announce in inbox *)
let kick_inbox_announce env (req : ap_announce) =
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let s = status_of_announce env req in
  Worker.Distribute.kick env s;
  let s' = Db.e (Model.Status.get_one ~id:(Option.get s#reblog_of_id)) in
  let dst = Db.e (Model.Account.get_one ~id:s'#account_id) in
  if Model.Account.is_local dst then
    let src = Db.e (Model.Account.get_one ~id:s#account_id) in
    Worker.Local_notify.kick env
      ~activity_id:(Model.Status.ID.to_int s#id)
      ~activity_type:`Status ~typ:`reblog ~src ~dst

(* Recv Like in inbox *)
let kick_inbox_like env (req : ap_like) =
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let f = favourite_of_like req in
  let src = Db.e (Model.Account.get_one ~id:f#account_id) in
  let s = Db.e (Model.Status.get_one ~id:f#status_id) in
  let dst = Db.e (Model.Account.get_one ~id:s#account_id) in
  Worker.Local_notify.kick env
    ~activity_id:(Model.Favourite.ID.to_int f#id)
    ~activity_type:`Favourite ~typ:`favourite ~src ~dst

let kick_inbox_delete env (req : ap_delete) =
  match Db.e (Model.Account.get_one ~uri:req.actor) with
  | exception Sqlx.Error.NoRowFound ->
      (* Unknown account; just ignore *)
      ()
  | account -> (
      (* FIXME: Support deletion of accounts *)
      match req.obj |> of_yojson |> get_tombstone with
      | (exception _) | None -> (* Unknown object; just ignore *) ()
      | Some tomb -> (
          match Db.(e Status.(get_many ~uri:tomb.id)) with
          | [] -> ()
          | [ status ] ->
              Worker.Removal.kick env ~account_id:account#id
                ~status_id:status#id
          | _ :: _ -> failwith "Internal error: not unique status uri"))

let kick_inbox_update_person (r : ap_person) =
  let a = Db.e (Model.Account.get_one ~uri:r.id) in
  let a = Activity.model_account_of_person ~original:a r in
  Db.(e @@ Account.save_one a) |> ignore

(* Recv POST /users/:name/inbox *)
let post env req =
  match Activity.verify_activity_json env req with
  | body, Error (`AccountNotFound | `AccountIsLocal) ->
      Logq.err (fun m -> m "account not found or account is local: %s" body);
      Yume.Server.respond ~status:`Accepted ""
  | body, Error (`VerifFailure e) ->
      Logq.err (fun m ->
          m "verification failure of activity json: %s: %s"
            (match e with
            | `AlgorithmNotImplemented -> "algorithm not implemented"
            | `Msg msg -> msg)
            body);
      Yume.Server.respond ~status:`Unauthorized ""
  | body, Ok () -> (
      try
        (*Logq.debug (fun m -> m ">>>>>>>>\n%s" body);*)
        (match Yojson.Safe.from_string body |> of_yojson with
        | Accept r -> kick_inbox_accept env r
        | Announce r -> kick_inbox_announce env r
        | Create r -> kick_inbox_create env r
        | Delete r -> kick_inbox_delete env r
        | Follow r -> kick_inbox_follow env r
        | Like r -> kick_inbox_like env r
        | Undo { obj = Follow v; _ } -> kick_inbox_undo_follow env v
        | Undo { obj = Like v; _ } -> kick_inbox_undo_like env v
        | Undo { obj = Announce v; _ } -> kick_inbox_undo_announce env v
        | Update { obj = Person r; _ } -> kick_inbox_update_person r
        | _ -> failwith "activity not implemented");
        Yume.Server.respond ~status:`Accepted ""
      with e ->
        Logq.err (fun m ->
            m
              "Failed to handle inboxed message; the response and request will \
               be printed in log:\n\
               %s\n\
               %s"
              (Printexc.to_string e)
              (Printexc.get_backtrace ()));
        Yume.Server.respond ~status:`Accepted ~tags:[ "log" ] "")
