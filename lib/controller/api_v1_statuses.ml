open Activity
open Util

(* Recv POST /api/v1/statuses *)
type post_api_v1_statuses_res = {
  id : string;
  created_at : string;
  content : string;
  uri : string;
}
[@@deriving make, yojson { strict = false }]

let post req =
  let%lwt self_id = Httpx.authenticate_user req in
  let status = req |> Httpx.query "status" in

  let now = Ptime.now () in
  let%lwt self = Db.Account.get_one ~id:self_id () in
  (* Insert status *)
  let%lwt s =
    Db.Status.(
      make ~id:0 ~text:status ~uri:"" ~created_at:now ~updated_at:now
        ~account_id:self_id
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
  make_post_api_v1_statuses_res ~id:(string_of_int s.id)
    ~created_at:(Ptime.to_rfc3339 now) ~content:s.text ~uri:s.uri
  |> post_api_v1_statuses_res_to_yojson |> Yojson.Safe.to_string
  |> Http.respond ~headers:[ Helper.content_type_app_json ]
