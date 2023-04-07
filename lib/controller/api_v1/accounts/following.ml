open Lwt.Infix
open Helper

type params = {
  self_id : Model.Account.ID.t option;
  id : Model.Account.ID.t;
  limit : int;
  max_id : Model.Follow.ID.t option;
  since_id : Model.Follow.ID.t option;
}

let string_to_follow_id s = s |> int_of_string |> Model.Follow.ID.of_int

let parse_req req =
  let open Httpq.Server in
  let%lwt self_id =
    may_authenticate_account req >|= fun a -> a |> Option.map (fun a -> a#id)
  in
  let id = req |> param ":id" |> int_of_string |> Model.Account.ID.of_int in
  let%lwt limit = req |> query ~default:"40" "limit" >|= int_of_string in
  let limit = min limit 80 in
  let%lwt max_id =
    req |> query_opt "max_id" >|= Option.map string_to_follow_id
  in
  let%lwt since_id =
    req |> query_opt "since_id" >|= Option.map string_to_follow_id
  in
  Lwt.return { id; self_id; max_id; since_id; limit }

let respond_account_list accts =
  accts
  |> List.map (fun (a : Db.Account.t) -> a#id)
  |> Entity.load_accounts_from_db
  >|= List.map Entity.yojson_of_account
  >|= (fun l -> `List l)
  >>= respond_yojson

let get_following req =
  (* FIXME: Return Link header for pagination *)
  let%lwt { id; self_id; max_id; since_id; limit } = parse_req req in
  Db.(e @@ get_following ~id ~self_id ~max_id ~since_id ~limit)
  >>= respond_account_list

let get_followers req =
  (* FIXME: Return Link header for pagination *)
  let%lwt { id; self_id; max_id; since_id; limit } = parse_req req in
  Db.(e @@ get_followers ~id ~self_id ~max_id ~since_id ~limit)
  >>= respond_account_list
