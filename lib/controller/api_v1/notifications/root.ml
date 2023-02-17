open Entity
open Helper
open Lwt.Infix

type params = {
  self_id : int;
  limit : int;
  max_id : int option;
  since_id : int option;
}

let parse_req req =
  let open Httpq.Server in
  let%lwt self_id = authenticate_user req in
  let limit = req |> query ~default:"15" "limit" |> int_of_string in
  let limit = min limit 30 in
  let max_id = req |> query_opt "max_id" |> Option.map int_of_string in
  let since_id = req |> query_opt "since_id" |> Option.map int_of_string in
  Lwt.return { self_id; max_id; since_id; limit }

let get req =
  let%lwt { self_id; max_id; since_id; limit } = parse_req req in

  Db.get_notifications ~account_id:self_id ~max_id ~since_id ~limit
  >>= Lwt_list.map_p (Entity.make_notification_from_model ~self_id)
  >|= List.map yojson_of_notification
  >|= (fun l -> `List l)
  >>= respond_yojson
