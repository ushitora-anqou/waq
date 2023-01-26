(* Recv POST /api/v1/accounts/:id/follow *)
type post_api_v1_accounts_follow_res = {
  id : string;
  following : bool;
  showing_reblogs : bool;
  notifying : bool;
  followed_by : bool;
  blocking : bool;
  blocked_by : bool;
  muting : bool;
  muting_notifications : bool;
  requested : bool;
  domain_blocking : bool;
  endorsed : bool;
}
[@@deriving make, yojson { strict = false }]

(* Recv POST /api/v1/accounts/:id/unfollow *)
(* Use post_api_v1_accounts_follow_res as a result *)
let post req =
  let%lwt self_id = Httpx.authenticate_user req in
  let id = Httpx.(req |> param ":id" |> int_of_string) in

  (* Check if accounts are valid *)
  let%lwt self = Db.Account.get_one ~id:self_id () in
  let%lwt acc = Db.Account.get_one ~id () in
  (* Check if followed *)
  let%lwt f =
    Db.(
      Follow.get_one ~account_id:self_id ~target_account_id:id ()
      |> maybe_no_row)
  in
  (* If valid, send Undo of Follow to the server *)
  if f <> None then Service.Unfollow.kick self acc (Option.get f);
  (* Return the result to the client *)
  make_post_api_v1_accounts_follow_res ~id:(string_of_int id) ~following:false
    ~showing_reblogs:false ~notifying:false ~followed_by:false ~blocking:false
    ~blocked_by:false ~muting:false ~muting_notifications:false ~requested:false
    ~domain_blocking:false ~endorsed:false
  |> post_api_v1_accounts_follow_res_to_yojson |> Yojson.Safe.to_string
  |> Http.respond ~headers:[ Helper.content_type_app_json ]
