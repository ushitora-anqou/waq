open Helper

type t = { ancestors : Entity.status list; descendants : Entity.status list }
[@@deriving make, yojson]

let get req =
  let%lwt self_id = may_authenticate_user req in
  let status_id =
    req |> Httpq.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  let%lwt ancestors = Db.(e (Status.get_ancestors status_id) |> maybe_no_row) in
  let%lwt descendants =
    Db.(e (Status.get_descendants status_id) |> maybe_no_row)
  in
  match (ancestors, descendants) with
  | None, _ | _, None -> Httpq.Server.raise_error_response `Not_found
  | Some ancestors, Some descendants ->
      let%lwt ancestors =
        ancestors
        |> List.map (fun (s : Db.Status.t) -> s#id)
        |> Entity.serialize_statuses ?self_id
      in
      let%lwt descendants =
        descendants
        |> List.map (fun (s : Db.Status.t) -> s#id)
        |> Entity.serialize_statuses ?self_id
      in
      make ~ancestors ~descendants ()
      |> yojson_of_t |> Yojson.Safe.to_string
      |> Httpq.Server.respond ~headers:[ content_type_app_json ]
