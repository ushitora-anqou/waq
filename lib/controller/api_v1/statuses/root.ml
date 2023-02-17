open Entity
open Util

(* GET /api/v1/statuses/:id *)
let get req =
  let status_id = req |> Httpq.Server.param ":id" |> int_of_string in
  let%lwt self_id = Helper.may_authenticate_user req in
  match%lwt Db.(Status.get_one ~id:status_id () |> maybe_no_row) with
  | None -> Httpq.Server.raise_error_response `Not_found
  | Some s ->
      let%lwt s = make_status_from_model ?self_id s in
      s |> yojson_of_status |> Yojson.Safe.to_string
      |> Httpq.Server.respond ~headers:[ Helper.content_type_app_json ]

(* Recv POST /api/v1/statuses *)
let post req =
  let%lwt self_id = Helper.authenticate_user req in
  let status = req |> Httpq.Server.query "status" in
  let in_reply_to_id =
    req |> Httpq.Server.query_opt "in_reply_to_id" |> Option.map int_of_string
  in

  let now = Ptime.now () in
  (* Insert status *)
  let%lwt s =
    Db.Status.(
      make ~id:0 ~text:status ~created_at:now ~updated_at:now
        ~account_id:self_id ?in_reply_to_id ()
      |> save_one_with_uri)
  in
  (* Deliver the status to others *)
  Worker.Distribute.kick s;%lwt
  (* Return the result to the client *)
  let%lwt s = make_status_from_model ~self_id s in
  s |> yojson_of_status |> Yojson.Safe.to_string
  |> Httpq.Server.respond ~headers:[ Helper.content_type_app_json ]
