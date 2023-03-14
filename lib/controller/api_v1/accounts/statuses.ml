open Lwt.Infix
open Helper

type params = {
  self_id : Model.Account.ID.t option;
  id : Model.Account.ID.t;
  limit : int;
  max_id : Model.Status.ID.t option;
  since_id : Model.Status.ID.t option;
  exclude_replies : bool;
}

let parse_req req =
  let open Httpq.Server in
  let%lwt self_id = may_authenticate_user req in
  let id = req |> param ":id" |> int_of_string |> Model.Account.ID.of_int in
  let limit = req |> query ~default:"20" "limit" |> int_of_string in
  let limit = min limit 40 in
  let max_id =
    req |> query_opt "max_id"
    |> Option.map (fun s -> s |> int_of_string |> Model.Status.ID.of_int)
  in
  let since_id =
    req |> query_opt "since_id"
    |> Option.map (fun s -> s |> int_of_string |> Model.Status.ID.of_int)
  in
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
  Db.(e @@ account_statuses ~id ~limit ~max_id ~since_id ~exclude_replies)
  >|= List.map (fun (s : Db.Status.t) -> s#id)
  >>= Entity.serialize_statuses ?self_id
  >|= List.map Entity.yojson_of_status
  >|= (fun l -> `List l)
  >>= respond_yojson
