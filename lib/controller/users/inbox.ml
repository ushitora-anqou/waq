open Util
open Activity

(* Recv Follow in inbox *)
let kick_inbox_follow (req : ap_follow) =
  let src, dst =
    match (req.actor, req.obj) with
    | s, d when is_my_domain d -> (s, d)
    | _ -> Httpq.Server.raise_error_response `Bad_request
  in
  let%lwt src = fetch_account (`Uri src) in
  match%lwt Db.Account.get_one ~uri:dst () with
  | exception Sql.NoRowFound -> Httpq.Server.raise_error_response `Bad_request
  | dst ->
      Job.kick ~name:__FUNCTION__ @@ fun () ->
      let%lwt f =
        match%lwt
          Db.Follow.get_one ~account_id:src.id ~target_account_id:dst.id ()
        with
        | f -> Lwt.return f
        | exception Sql.NoRowFound ->
            (* Insert to table 'follows' *)
            let now = Ptime.now () in
            let%lwt f =
              Db.Follow.(
                make ~id:0 ~created_at:now ~updated_at:now ~account_id:src.id
                  ~target_account_id:dst.id ~uri:req.id
                |> save_one)
            in
            Worker.Local_notify.kick ~activity_id:f.id ~activity_type:"Follow"
              ~src ~dst ~typ:"follow";%lwt
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
  match%lwt Db.FollowRequest.get_one ~uri () with
  | exception Sql.NoRowFound -> Httpq.Server.raise_error_response `Bad_request
  | r ->
      Job.kick ~name:__FUNCTION__ @@ fun () ->
      let now = Ptime.now () in
      Db.FollowRequest.delete ~id:r.id () |> ignore_lwt;%lwt
      Db.Follow.(
        make ~id:0 ~account_id:r.account_id
          ~target_account_id:r.target_account_id ~uri ~created_at:now
          ~updated_at:now
        |> save_one)
      |> ignore_lwt

let kick_inbox_undo_like (l : ap_like) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt { id; _ } = favourite_of_like ~must_already_exist:true l in
  Db.Favourite.delete ~id ()

(* Recv Undo in inbox *)
let kick_inbox_undo (req : ap_undo) =
  match req.obj with
  | Follow { id; _ } ->
      Job.kick ~name:__FUNCTION__ @@ fun () -> Db.Follow.delete ~uri:id ()
  | Like l -> kick_inbox_undo_like l
  | _ -> Httpq.Server.raise_error_response `Bad_request

(* Recv Create in inbox *)
let kick_inbox_create (req : ap_create) =
  let note =
    match req with
    | { obj = Note note; _ } -> note
    | _ -> Httpq.Server.raise_error_response `Bad_request
  in
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt s = status_of_note note in
  Worker.Distribute.kick s

(* Recv Announce in inbox *)
let kick_inbox_announce (req : ap_announce) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt s = status_of_announce req in
  Worker.Distribute.kick s;%lwt
  let%lwt src = Db.Account.get_one ~id:s.account_id () in
  let%lwt s' = Db.Status.get_one ~id:(Option.get s.reblog_of_id) () in
  let%lwt dst = Db.Account.get_one ~id:s'.account_id () in
  Worker.Local_notify.kick ~activity_id:s.id ~activity_type:"Status"
    ~typ:"reblog" ~src ~dst

(* Recv Like in inbox *)
let kick_inbox_like (req : ap_like) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  let%lwt _fav = favourite_of_like req in
  (* FIXME: Send notification *)
  Lwt.return_unit

(* Recv POST /users/:name/inbox *)
let post req =
  let body = Httpq.Server.body req in
  let j = Yojson.Safe.from_string body in
  (match of_yojson j with
  | Accept r -> kick_inbox_accept r
  | Announce r -> kick_inbox_announce r
  | Follow r -> kick_inbox_follow r
  | Undo r -> kick_inbox_undo r
  | Create r -> kick_inbox_create r
  | Like r -> kick_inbox_like r
  | _ | (exception _) ->
      Logq.warn (fun m -> m "Ignoring inbox message:\n%s" body);
      Lwt.return_unit);%lwt
  Httpq.Server.respond ~status:`Accepted ""
