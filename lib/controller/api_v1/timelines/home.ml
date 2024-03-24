open Helper

let parse_req req =
  let open Yume.Server in
  let self = authenticate_account req in
  let limit = req |> query ~default:"20" "limit" |> int_of_string in
  let limit = min limit 40 in
  let max_id = req |> query_opt "max_id" |> Option.map string_to_status_id in
  let since_id =
    req |> query_opt "since_id" |> Option.map string_to_status_id
  in
  (self#id, max_id, since_id, limit)

let get _env req =
  (* Parse the request *)
  let self_id, max_id, since_id, limit = parse_req req in

  (* Retrieve the result from DB *)
  let result =
    Db.(e @@ home_timeline ~id:self_id ~limit ~max_id ~since_id)
    |> List.map (fun (s : Db.Status.t) -> s#id)
    |> Entity.load_statuses_from_db ~self_id
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
