[%%sqlx.schemas
module rec Account = struct
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
  val followers_url : string
  val avatar_remote_url : string option
  val header_remote_url : string
  val stat : AccountStat.t [@@not_column]
end

and AccountStat = struct
  name "account_stats"

  val account_id : Account.ID.t
  val statuses_count : int
  val following_count : int
  val followers_count : int
  val last_status_at : Ptime.t option
end

and Status = struct
  name "statuses"

  val uri : string
  val text : string
  val deleted_at : Ptime.t option
  val in_reply_to_id : ID.t option
  val reblog_of_id : ID.t option
  val account_id : Account.ID.t
  val stat : StatusStat.t [@@not_column]
  val reblogged : bool [@@not_column] [@@preload_spec: Account.ID.t option]
  val favourited : bool [@@not_column] [@@preload_spec: Account.ID.t option]
  val attachments : MediaAttachment.t list [@@not_column]
end

and StatusStat = struct
  name "status_stats"

  val status_id : Status.ID.t
  val replies_count : int
  val reblogs_count : int
  val favourites_count : int
end

and MediaAttachment = struct
  name "media_attachments"

  val status_id : int option (* Status.ID.t option *)
  val account_id : Account.ID.t option
  val remote_url : string
  val type_ : int [@@column "type"]
end

and User = struct
  name "users"

  val email : string
  val encrypted_password : string
  val account_id : Account.ID.t
end

and Follow = struct
  name "follows"

  val account_id : Account.ID.t
  val target_account_id : Account.ID.t
  val uri : string
end

and FollowRequest = struct
  name "follow_requests"

  val account_id : Account.ID.t
  val target_account_id : Account.ID.t
  val uri : string
end

and OAuthApplication = struct
  name "oauth_applications"

  val name : string
  val uid : string
  val secret : string
  val redirect_uri : string
  val scopes : string
end

and OAuthAccessGrant = struct
  name "oauth_access_grants"

  val token : string
  val expires_in : int
  val redirect_uri : string
  val scopes : string option
  val application_id : OAuthApplication.ID.t option
  val resource_owner_id : User.ID.t option
end

and OAuthAccessToken = struct
  name "oauth_access_tokens"

  val token : string
  val scopes : string option
  val application_id : OAuthApplication.ID.t option
  val resource_owner_id : User.ID.t option
end

and Favourite = struct
  name "favourites"

  val account_id : Account.ID.t
  val status_id : Status.ID.t
end

and Notification = struct
  name "notifications"

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

  val activity_id : int
  val activity_type : activity_type_t
  val account_id : Account.ID.t
  val from_account_id : Account.ID.t
  val typ : typ_t option [@@column "type"]

  val target_status : Status.t option
    [@@not_column] [@@preload_spec: Status.preload_spec]
end]
