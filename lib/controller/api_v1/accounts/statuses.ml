open Entity
open Lwt.Infix
open Helper

type params = {
  self_id : int option;
  id : int;
  limit : int;
  max_id : int option;
  since_id : int option;
  exclude_replies : bool;
}

let parse_req req =
  let open Httpq.Server in
  let%lwt self_id = may_authenticate_user req in
  let id = req |> param ":id" |> int_of_string in
  let limit = req |> query ~default:"20" "limit" |> int_of_string in
  let limit = min limit 40 in
  let max_id = req |> query_opt "max_id" |> Option.map int_of_string in
  let since_id = req |> query_opt "since_id" |> Option.map int_of_string in
  let exclude_replies =
    req
    |> query_opt "exclude_replies"
    |> Option.map bool_of_string
    |> Option.value ~default:false
  in
  Lwt.return { id; self_id; max_id; since_id; limit; exclude_replies }

let get req =
  let%lwt { self_id; id; limit; max_id; since_id; exclude_replies } =
    parse_req req
  in
  Db.account_statuses ~id ~limit ~max_id ~since_id ~exclude_replies
  >>= Lwt_list.map_p (make_status_from_model ?self_id)
  >|= List.map status_to_yojson
  >|= (fun l -> `List l)
  >>= respond_yojson
