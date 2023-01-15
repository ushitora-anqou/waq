open Activity
open Util

(* Recv POST /api/v1/statuses *)
type post_api_v1_statuses_res = {
  id : string;
  created_at : string;
  content : string;
}
[@@deriving make, yojson { strict = false }]

let post self_id status =
  let now = Ptime.now () in
  let%lwt self = Db.Account.get ~by:(`id self_id) in
  (* Insert status *)
  let%lwt s =
    Db.Status.make ~id:0 ~text:status ~uri:"" ~created_at:now ~updated_at:now
      ~account_id:self_id
    |> Db.Status.insert
  in
  (* Update status URI using its ID *)
  let%lwt s =
    { s with uri = self.uri ^/ "statuses" ^/ string_of_int s.id }
    |> Db.Status.update_uri
  in
  (* Send followers the status *)
  let%lwt followers = Db.Follow.get_many ~by:(`target_account_id self_id) in
  followers
  |> List.iter (fun (f : Db.Follow.t) ->
         Service_create_note.kick f.account_id s);
  (* Return the result to the client *)
  make_post_api_v1_statuses_res ~id:(string_of_int s.id)
    ~created_at:(Ptime.to_rfc3339 now) ~content:s.text
  |> post_api_v1_statuses_res_to_yojson |> Yojson.Safe.to_string |> Result.ok
  |> Lwt.return
