open Helper
open Lwt.Infix

let parse_req req =
  let open Httpq.Server in
  let%lwt self = authenticate_account req in
  let%lwt limit = req |> query ~default:"20" "limit" >|= int_of_string in
  let limit = min limit 40 in
  let%lwt max_id =
    req |> query_opt "max_id" >|= Option.map string_to_status_id
  in
  let%lwt since_id =
    req |> query_opt "since_id" >|= Option.map string_to_status_id
  in
  Lwt.return (self#id, max_id, since_id, limit)

let construct_link_header ~url_prefix ~limit ~max_id ~min_id =
  let next =
    Printf.sprintf "%s?limit=%d&max_id=%s" url_prefix limit max_id
    |> Config.absolute_url
  in
  let prev =
    Printf.sprintf "%s?limit=%d&min_id=%s" url_prefix limit min_id
    |> Config.absolute_url
  in
  let v = Printf.sprintf {|<%s>; rel="next", <%s>; rel="prev"|} next prev in
  (`Link, v)

let get req =
  (* Parse the request *)
  let%lwt self_id, max_id, since_id, limit = parse_req req in

  (* Retrieve the result from DB *)
  let%lwt result =
    Db.(e @@ home_timeline ~id:self_id ~limit ~max_id ~since_id)
    >|= List.map (fun (s : Db.Status.t) -> s#id)
    >>= Entity.load_statuses_from_db ~self_id
  in

  (* Construct HTTP link header *)
  let headers =
    match result with
    | [] -> None
    | _ ->
        Some
          [
            construct_link_header ~url_prefix:"/api/v1/timelines/home" ~limit
              ~max_id:List.(result |> rev |> hd).id
              ~min_id:List.(result |> hd).id;
          ]
  in

  (* Return the result with the header *)
  result
  |> List.map Entity.yojson_of_status
  |> (fun l -> `List l)
  |> respond_yojson ?headers
