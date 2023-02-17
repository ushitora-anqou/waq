open Entity

type t = { ancestors : status list; descendants : status list }
[@@deriving make, yojson]

let get req =
  let%lwt self_id = Helper.may_authenticate_user req in
  let status_id = req |> Httpq.Server.param ":id" |> int_of_string in
  let%lwt ancestors = Db.(Status.get_ancestors status_id |> maybe_no_row) in
  let%lwt descendants = Db.(Status.get_descendants status_id |> maybe_no_row) in
  match (ancestors, descendants) with
  | None, _ | _, None -> Httpq.Server.raise_error_response `Not_found
  | Some ancestors, Some descendants ->
      let%lwt ancestors =
        ancestors |> Lwt_list.map_p (make_status_from_model ?self_id)
      in
      let%lwt descendants =
        descendants |> Lwt_list.map_p (make_status_from_model ?self_id)
      in
      make ~ancestors ~descendants ()
      |> yojson_of_t |> Yojson.Safe.to_string
      |> Httpq.Server.respond ~headers:[ Helper.content_type_app_json ]
