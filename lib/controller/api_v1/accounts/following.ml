open Entity
open Lwt.Infix
open Helper

type params = {
  self_id : int option;
  id : int;
  limit : int;
  max_id : int option;
  since_id : int option;
}

let parse_req req =
  let open Httpq.Server in
  let%lwt self_id = may_authenticate_user req in
  let id = req |> param ":id" |> int_of_string in
  let limit = req |> query ~default:"40" "limit" |> int_of_string in
  let limit = min limit 80 in
  let max_id = req |> query_opt "max_id" |> Option.map int_of_string in
  let since_id = req |> query_opt "since_id" |> Option.map int_of_string in
  Lwt.return { id; self_id; max_id; since_id; limit }

let respond_account_list accts =
  Lwt_list.map_p make_account_from_model accts
  >|= List.map account_to_yojson
  >|= (fun l -> `List l)
  >>= respond_yojson

let get_following req =
  (* FIXME: Return Link header for pagination *)
  let%lwt { id; self_id; max_id; since_id; limit } = parse_req req in
  Db.get_following ~id ~self_id ~max_id ~since_id ~limit
  >>= respond_account_list

let get_followers req =
  (* FIXME: Return Link header for pagination *)
  let%lwt { id; self_id; max_id; since_id; limit } = parse_req req in
  Db.get_followers ~id ~self_id ~max_id ~since_id ~limit
  >>= respond_account_list
