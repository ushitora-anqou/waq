open Common

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

let post self_id id =
  (* Check if accounts are valid *)
  let%lwt self = Db.Account.get ~by:(`id self_id) in
  let%lwt acc = Db.Account.get ~by:(`id id) in
  (* Check if already followed or follow-requested *)
  let%lwt f = Db.(Follow.get ~by:(`accounts (self_id, id)) |> maybe_no_row) in
  let%lwt frq =
    Db.(FollowRequest.get ~by:(`accounts (self_id, id)) |> maybe_no_row)
  in
  (* If valid, send Follow to the server *)
  Log.debug (fun m ->
      m ">>>>>>>>>> %s %s"
        (if f = None then "None" else "Some")
        (if frq = None then "None" else "Some"));
  if f = None && frq = None then Service.Follow.kick self acc;
  (* Return the result to the client *)
  make_post_api_v1_accounts_follow_res ~id:(string_of_int id) ~following:true
    ~showing_reblogs:true ~notifying:false ~followed_by:false ~blocking:false
    ~blocked_by:false ~muting:false ~muting_notifications:false ~requested:false
    ~domain_blocking:false ~endorsed:false
  |> post_api_v1_accounts_follow_res_to_yojson |> Yojson.Safe.to_string
  |> Result.ok |> Lwt.return
