open Helper

type params = {
  self_id : Model.Account.ID.t option;
  id : Model.Account.ID.t;
  limit : int;
  max_id : Model.Status.ID.t option;
  since_id : Model.Status.ID.t option;
  exclude_replies : bool;
  pinned : bool;
}

let parse_req req =
  let open Yume.Server in
  let self_id =
    may_authenticate_account req |> fun a -> a |> Option.map (fun a -> a#id)
  in
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
  let pinned =
    req |> query_opt "pinned" |> Option.map bool_of_string
    |> Option.value ~default:false
  in
  { id; self_id; max_id; since_id; limit; exclude_replies; pinned }

let get _ req =
  let { self_id; id; limit; max_id; since_id; exclude_replies; pinned } =
    parse_req req
  in
  let result =
    if pinned then []
    else
      Db.(e @@ account_statuses ~id ~limit ~max_id ~since_id ~exclude_replies)
      |> List.map (fun (s : Db.Status.t) -> s#id)
      |> Entity.load_statuses_from_db ?self_id
  in
  let headers =
    match result with
    | [] -> None
    | _ ->
        let url_prefix =
          "/api/v1/accounts/"
          ^ (id |> Model.Account.ID.to_int |> string_of_int)
          ^ "/statuses"
        in
        Some
          [
            construct_link_header ~url_prefix ~limit
              ~max_id:List.(result |> rev |> hd).id
              ~min_id:List.(result |> hd).id;
          ]
  in
  result
  |> List.map Entity.yojson_of_status
  |> (fun l -> `List l)
  |> respond_yojson ?headers
