[@@@warning "-39"]

module Account = struct
  [%%sqlx.schema
  name "accounts"

  val username : string
  val domain : string option
  val private_key : string option
  val public_key : string
  val display_name : string
  val uri : string
  val url : string option
  val inbox_url : string
  val outbox_url : string
  val shared_inbox_url : string
  val followers_url : string]

  let is_local (x : t) = Option.is_none x#domain_opt
  let is_remote (x : t) = Option.is_some x#domain_opt

  let preferred_inbox_url (a : t) =
    match (a#inbox_url, a#shared_inbox_url) with s, "" -> s | _, s -> s

  let preferred_inbox_urls (accts : t list) =
    accts |> List.map preferred_inbox_url |> List.sort_uniq compare
end

module AccountStat = struct
  [%%sqlx.schema
  name "account_stats"

  val account_id : Account.ID.t
  val statuses_count : int
  val following_count : int
  val followers_count : int
  val last_status_at : Ptime.t option]
end

module Status = struct
  [%%sqlx.schema
  name "statuses"

  val uri : string
  val text : string
  val deleted_at : Ptime.t option
  val in_reply_to_id : ID.t option
  val reblog_of_id : ID.t option
  val account_id : Account.ID.t]
end

module StatusStat = struct
  [%%sqlx.schema
  name "status_stats"

  val status_id : Status.ID.t
  val replies_count : int
  val reblogs_count : int
  val favourites_count : int]
end

module Follow = struct
  [%%sqlx.schema
  name "follows"

  val account_id : Account.ID.t
  val target_account_id : Account.ID.t
  val uri : string]
end

module FollowRequest = struct
  [%%sqlx.schema
  name "follow_requests"

  val account_id : Account.ID.t
  val target_account_id : Account.ID.t
  val uri : string]
end

module OAuthApplication = struct
  [%%sqlx.schema
  name "oauth_applications"

  val name : string
  val uid : string
  val secret : string
  val redirect_uri : string
  val scopes : string]
end

module OAuthAccessGrant = struct
  [%%sqlx.schema
  name "oauth_access_grants"

  val token : string
  val expires_in : int
  val redirect_uri : string
  val scopes : string option
  val application_id : OAuthApplication.ID.t option
  val resource_owner_id : Account.ID.t option]
end

module OAuthAccessToken = struct
  [%%sqlx.schema
  name "oauth_access_token"

  val token : string
  val scopes : string option
  val application_id : OAuthApplication.ID.t option
  val resource_owner_id : Account.ID.t option]
end

module Favourite = struct
  [%%sqlx.schema
  name "favourites"

  val account_id : Account.ID.t
  val status_id : Status.ID.t]
end

module Notification = struct
  type activity_type_t = [ `Status | `Favourite | `Follow ]

  let activity_type_t_to_string : activity_type_t -> string = function
    | `Status -> "Status"
    | `Favourite -> "Favourite"
    | `Follow -> "Follow"

  let activity_type_t_of_string : string -> activity_type_t = function
    | "Status" -> `Status
    | "Favourite" -> `Favourite
    | "Follow" -> `Follow
    | _ -> failwith "activity_type_t_of_string: invalid input"

  type typ_t = [ `reblog | `favourite | `follow ]

  let typ_t_to_string : typ_t -> string = function
    | `reblog -> "reblog"
    | `favourite -> "favourite"
    | `follow -> "follow"

  let typ_t_of_string : string -> typ_t = function
    | "reblog" -> `reblog
    | "favourite" -> `favourite
    | "follow" -> `follow
    | _ -> failwith "type_t_of_string: invalid input"

  [%%sqlx.schema
  name "notifications"

  val activity_id : int
  val activity_type : activity_type_t
  val account_id : Account.ID.t
  val from_account_id : Account.ID.t
  val typ : typ_t option [@@column "type"]
  val target_status : Status.t [@@not_column]]

  let load_target_status (ns : t list) c =
    let open Lwt.Infix in
    let map_fst_sort_uniq r = r |> List.map fst |> List.sort_uniq compare in

    (* Load favourites *)
    ( ns
    |> List.filter_map (fun n ->
           match (n#activity_type, n#typ) with
           | `Favourite, _ | _, `favourite ->
               Some (Favourite.ID.of_int n#activity_id, n#set_target_status)
           | _ -> None)
    |> fun r ->
      Favourite.select ~id:(`In (map_fst_sort_uniq r)) c
      >|= List.map (fun x -> (x#status_id, List.assoc x#id r))
      >>= fun r ->
      Status.select ~id:(`In (map_fst_sort_uniq r)) c
      >|= List.iter (fun x -> (List.assoc x#id r) x) );%lwt

    (* Load reblogs *)
    ( ns
    |> List.filter_map (fun n ->
           match (n#activity_type, n#typ) with
           | `Status, _ | _, `reblog ->
               Some (Status.ID.of_int n#activity_id, n#set_target_status)
           | _ -> None)
    |> fun r ->
      Status.select ~id:(`In (map_fst_sort_uniq r)) c
      >|= List.iter (fun x -> (List.assoc x#id r) x) );%lwt

    Lwt.return_unit
end
