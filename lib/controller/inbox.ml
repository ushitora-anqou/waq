open Common
open Util
open Activity

(* Recv Follow in inbox *)
let kick_inbox_follow (req : ap_inbox) =
  assert (req.typ = "Follow");
  let src, dst =
    match (req.actor, req.obj) with
    | `String s, `String d when is_my_domain d -> (s, d)
    | _ -> raise Bad_request
  in
  let%lwt src = fetch_account (`Uri src) in
  match%lwt Db.get_account_by_uri dst with
  | None -> raise Bad_request
  | Some dst ->
      Job.kick_lwt ~name:__FUNCTION__ @@ fun () ->
      let%lwt f =
        match%lwt
          Db.get_follow_by_accounts ~account_id:src.id ~target_account_id:dst.id
        with
        | Some f -> Lwt.return f
        | None ->
            (* Insert to table 'follows' *)
            let now = Ptime.now () in
            Db.make_follow ~id:0 ~created_at:now ~updated_at:now
              ~account_id:src.id ~target_account_id:dst.id ~uri:req.id
            |> Db.insert_follow
      in
      (* Send 'Accept' *)
      Service.Accept.kick ~f ~followee:dst ~follower:src;
      Lwt.return_unit

(* Recv Accept in inbox *)
let kick_inbox_accept (req : ap_inbox) =
  assert (req.typ = "Accept");
  let uri =
    match req.obj with
    | `Assoc l -> (
        match l |> List.assoc_opt "id" with
        | Some (`String uri) -> uri
        | _ -> raise Bad_request)
    | _ -> raise Bad_request
  in
  match%lwt Db.get_follow_request_by_uri uri with
  | None -> raise Bad_request
  | Some r ->
      Job.kick_lwt ~name:__FUNCTION__ @@ fun () ->
      let now = Ptime.now () in
      Db.delete_follow_request r.id |> ignore_lwt;%lwt
      Db.make_follow ~id:0 ~account_id:r.account_id
        ~target_account_id:r.target_account_id ~uri ~created_at:now
        ~updated_at:now
      |> Db.insert_follow |> ignore_lwt

(* Recv Undo in inbox *)
let kick_inbox_undo (req : ap_inbox) =
  assert (req.typ = "Undo");
  let obj = ap_inbox_no_context_of_yojson req.obj |> Result.get_ok in
  match obj.typ with
  | "Follow" ->
      Job.kick_lwt ~name:__FUNCTION__ @@ fun () ->
      Db.delete_follow_by_uri obj.id
  | _ -> raise Bad_request

(* Recv POST /users/:name/inbox *)
let post body =
  match Yojson.Safe.from_string body |> ap_inbox_of_yojson with
  | Ok ({ typ = "Accept"; _ } as r) -> kick_inbox_accept r
  | Ok ({ typ = "Follow"; _ } as r) -> kick_inbox_follow r
  | Ok ({ typ = "Undo"; _ } as r) -> kick_inbox_undo r
  | _ -> Lwt.return_unit
