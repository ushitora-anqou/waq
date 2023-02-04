open Entity

let parse_req req =
  let open Httpq.Server in
  let open Helper in
  let%lwt self_id = Helper.authenticate_user req in
  let limit = req |> query ~default:"20" "limit" |> int_of_string in
  let limit = min limit 40 in
  let max_id = req |> query_opt "max_id" |> Option.map int_of_string in
  let since_id = req |> query_opt "since_id" |> Option.map int_of_string in
  Lwt.return (self_id, max_id, since_id, limit)

let get req =
  let%lwt self_id, max_id, since_id, limit = parse_req req in

  let conv (s : Db.Status.t) =
    let open Lwt.Infix in
    make_status_from_model s >|= status_to_yojson
  in
  let%lwt statuses = Db.home_timeline ~id:self_id ~limit ~max_id ~since_id in
  let%lwt statuses = Lwt_list.map_p conv statuses in
  `List statuses |> Yojson.Safe.to_string
  |> Httpq.Server.respond ~headers:[ Helper.content_type_app_json ]
