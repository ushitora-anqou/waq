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
  (* Send the status to self *)
  Service.Insert_to_feed.kick ~status_id:s.id ~user_id:self_id ~stream:`User;
  (* Send followers the status *)
  let%lwt followers = Db.Follow.get_many ~target_account_id:self_id () in
  followers
  |> Lwt_list.iter_p (fun (f : Db.Follow.t) ->
         let open Lwt.Infix in
         let%lwt a = Db.Account.get_one ~id:f.account_id () in
         match a.domain with
         | None ->
             (* local *)
             Db.User.get_one ~account_id:a.id () >|= fun u ->
             Service.Insert_to_feed.kick ~status_id:s.id ~user_id:u.id
               ~stream:`User
         | Some _ ->
             (* Remote *)
             Service.Create_note.kick f.account_id s |> Lwt.return);%lwt
  (* Return the result to the client *)
  make_post_api_v1_statuses_res ~id:(string_of_int s.id)
    ~created_at:(Ptime.to_rfc3339 now) ~content:s.text ~uri:s.uri
  |> post_api_v1_statuses_res_to_yojson |> Yojson.Safe.to_string
  |> Http.respond ~headers:[ Helper.content_type_app_json ]
