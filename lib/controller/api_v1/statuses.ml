open Activity
open Util

(* Recv POST /api/v1/statuses *)
let post req =
  let%lwt self_id = Helper.authenticate_user req in
  let status = req |> Httpq.Server.query "status" in
  let in_reply_to_id =
    req |> Httpq.Server.query_opt "in_reply_to_id" |> Option.map int_of_string
  in

  let now = Ptime.now () in
  let%lwt self = Db.Account.get_one ~id:self_id () in
  (* Insert status *)
  let%lwt s =
    Db.Status.(
      make ~id:0 ~text:status ~uri:"" ~created_at:now ~updated_at:now
        ~account_id:self_id ?in_reply_to_id ()
      |> save_one)
  in
  (* Update status URI using its ID *)
  let%lwt s =
    { s with uri = self.uri ^/ "statuses" ^/ string_of_int s.id }
    |> Db.Status.update_uri
  in
  (* Deliver the status to others *)
  Service.Distribute.kick s;
  (* Return the result to the client *)
  let%lwt s = make_status_from_model s in
  s |> status_to_yojson |> Yojson.Safe.to_string
  |> Httpq.Server.respond ~headers:[ Helper.content_type_app_json ]
