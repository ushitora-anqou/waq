open Helper
open Lwt.Infix

let parse_req req =
  let open Httpq.Server in
  let%lwt self_id = authenticate_user req in
  let limit = req |> query ~default:"20" "limit" |> int_of_string in
  let limit = min limit 40 in
  let max_id = req |> query_opt "max_id" |> Option.map string_to_status_id in
  let since_id =
    req |> query_opt "since_id" |> Option.map string_to_status_id
  in
  Lwt.return (self_id, max_id, since_id, limit)

let get req =
  let%lwt self_id, max_id, since_id, limit = parse_req req in

  Db.(e @@ home_timeline ~id:self_id ~limit ~max_id ~since_id)
  >|= List.map (fun (s : Db.Status.t) -> s#id)
  >>= Entity.serialize_statuses ~self_id
  >|= List.map Entity.yojson_of_status
  >|= (fun l -> `List l)
  >>= respond_yojson
