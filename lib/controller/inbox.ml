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
  match%lwt Db.Account.get ~by:(`uri dst) with
  | exception Sql.NoRowFound -> raise Bad_request
  | dst ->
      Job.kick_lwt ~name:__FUNCTION__ @@ fun () ->
      let%lwt f =
        match%lwt Db.Follow.get ~by:(`accounts (src.id, dst.id)) with
        | f -> Lwt.return f
        | exception Sql.NoRowFound ->
            (* Insert to table 'follows' *)
            let now = Ptime.now () in
            Db.Follow.make ~id:0 ~created_at:now ~updated_at:now
              ~account_id:src.id ~target_account_id:dst.id ~uri:req.id
            |> Db.Follow.insert
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
  match%lwt Db.FollowRequest.get ~by:(`uri uri) with
  | exception Sql.NoRowFound -> raise Bad_request
  | r ->
      Job.kick_lwt ~name:__FUNCTION__ @@ fun () ->
      let now = Ptime.now () in
      Db.FollowRequest.delete ~by:(`id r.id) |> ignore_lwt;%lwt
      Db.Follow.make ~id:0 ~account_id:r.account_id
        ~target_account_id:r.target_account_id ~uri ~created_at:now
        ~updated_at:now
      |> Db.Follow.insert |> ignore_lwt

(* Recv Undo in inbox *)
let kick_inbox_undo (req : ap_inbox) =
  assert (req.typ = "Undo");
  let obj = ap_inbox_no_context_of_yojson req.obj |> Result.get_ok in
  match obj.typ with
  | "Follow" ->
      Job.kick_lwt ~name:__FUNCTION__ @@ fun () ->
      Db.Follow.delete ~by:(`uri obj.id)
  | _ -> raise Bad_request

(* Recv POST /users/:name/inbox *)
let post body =
  match Yojson.Safe.from_string body |> ap_inbox_of_yojson with
  | Ok ({ typ = "Accept"; _ } as r) -> kick_inbox_accept r
  | Ok ({ typ = "Follow"; _ } as r) -> kick_inbox_follow r
  | Ok ({ typ = "Undo"; _ } as r) -> kick_inbox_undo r
  | _ -> Lwt.return_unit