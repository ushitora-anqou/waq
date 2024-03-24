open Helper

type params = {
  self_id : Model.Account.ID.t;
  limit : int;
  max_id : Model.Notification.ID.t option;
  since_id : Model.Notification.ID.t option;
}

let string_to_notification_id s =
  s |> int_of_string |> Model.Notification.ID.of_int

let parse_req req =
  let open Yume.Server in
  let self_id = authenticate_account req |> fun a -> a#id in
  let limit = req |> query ~default:"15" "limit" |> int_of_string in
  let limit = min limit 30 in
  let max_id =
    req |> query_opt "max_id" |> Option.map string_to_notification_id
  in
  let since_id =
    req |> query_opt "since_id" |> Option.map string_to_notification_id
  in
  { self_id; max_id; since_id; limit }

let get _ req =
  let { self_id; max_id; since_id; limit } = parse_req req in

  Db.(e @@ get_notifications ~account_id:self_id ~max_id ~since_id ~limit)
  |> List.map (fun (n : Db.Notification.t) -> n#id)
  |> Entity.load_notifications_from_db ~self_id
  |> List.map Entity.yojson_of_notification
  |> (fun l -> `List l)
  |> respond_yojson
