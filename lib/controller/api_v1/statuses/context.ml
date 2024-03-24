open Helper

type t = { ancestors : Entity.status list; descendants : Entity.status list }
[@@deriving make, yojson_of]

let get _ req =
  let self_id =
    may_authenticate_account req |> fun a -> a |> Option.map (fun x -> x#id)
  in
  let status_id =
    req |> Yume.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  match
    ( Db.(e (Status.get_ancestors status_id)),
      Db.(e (Status.get_descendants status_id)) )
  with
  | exception Sqlx.Error.NoRowFound ->
      Yume.Server.raise_error_response `Not_found
  | ancestors, descendants ->
      let ancestors =
        ancestors
        |> List.map (fun (s : Db.Status.t) -> s#id)
        |> Entity.load_statuses_from_db ?self_id
      in
      let descendants =
        descendants
        |> List.map (fun (s : Db.Status.t) -> s#id)
        |> Entity.load_statuses_from_db ?self_id
      in
      make ~ancestors ~descendants ()
      |> yojson_of_t |> Yojson.Safe.to_string
      |> Yume.Server.respond ~headers:[ content_type_app_json ]
