[%%sqlx.schemas
module rec Account = struct
  name "accounts"

  class type t =
    object
      val username : string
      val domain : string option
      val private_key : string option
      val public_key : string
      val display_name : string
      val note : string
      val uri : string
      val url : string option
      val inbox_url : string
      val outbox_url : string
      val shared_inbox_url : string
      val followers_url : string
      val avatar_remote_url : string option
      val header_remote_url : string
      val stat : AccountStat.t option [@@foreign_key `account_id]
      val user : User.t option [@@foreign_key `account_id]
    end
end

and AccountStat = struct
  name "account_stats"

  class type t =
    object
      val account_id : Account.ID.t
      val statuses_count : int
      val following_count : int
      val followers_count : int
      val last_status_at : Ptime.t option
    end
end

and Status = struct
  name "statuses"

  class type t =
    object
      val uri : string
      val text : string
      val deleted_at : Ptime.t option
      val in_reply_to_id : ID.t option
      val reblog_of_id : ID.t option
      val account_id : Account.ID.t
      val spoiler_text : string
      val stat : StatusStat.t option [@@foreign_key `status_id]
      val reblogged : bool [@@not_column] [@@preload_spec: Account.ID.t option]
      val favourited : bool [@@not_column] [@@preload_spec: Account.ID.t option]
      val attachments : MediaAttachment.t list [@@foreign_key `status_id]
      val mentions : Mention.t list [@@foreign_key `status_id]

      val preview_cards : PreviewCard.t list
      [@@not_column] [@@preload_spec: PreviewCard.preload_spec]
    end
end

and StatusStat = struct
  name "status_stats"

  class type t =
    object
      val status_id : Status.ID.t
      val replies_count : int
      val reblogs_count : int
      val favourites_count : int
    end
end

and MediaAttachment = struct
  name "media_attachments"

  class type t =
    object
      val status_id : Status.ID.t option
      val account_id : Account.ID.t option
      val remote_url : string
      val type_ : int [@@column "type"]
    end
end

and User = struct
  name "users"

  class type t =
    object
      val email : string
      val encrypted_password : string
      val account_id : Account.ID.t
    end
end

and Follow = struct
  name "follows"

  class type t =
    object
      val account_id : Account.ID.t
      val target_account_id : Account.ID.t
      val uri : string
    end
end

and FollowRequest = struct
  name "follow_requests"

  class type t =
    object
      val account_id : Account.ID.t
      val target_account_id : Account.ID.t
      val uri : string
    end
end

and OAuthApplication = struct
  name "oauth_applications"

  class type t =
    object
      val name : string
      val uid : string
      val secret : string
      val redirect_uri : string
      val scopes : string
    end
end

and OAuthAccessGrant = struct
  name "oauth_access_grants"

  class type t =
    object
      val token : string
      val expires_in : int
      val redirect_uri : string
      val scopes : string option
      val application_id : OAuthApplication.ID.t option
      val resource_owner_id : User.ID.t option
    end
end

and OAuthAccessToken = struct
  name "oauth_access_tokens"

  class type t =
    object
      val token : string
      val scopes : string option
      val application_id : OAuthApplication.ID.t option
      val resource_owner_id : User.ID.t option
    end
end

and Favourite = struct
  name "favourites"

  class type t =
    object
      val account_id : Account.ID.t
      val status_id : Status.ID.t
    end
end

and Notification = struct
  name "notifications"

  type activity_type_t = [ `Status | `Favourite | `Follow | `Mention ]

  let activity_type_t_to_string : activity_type_t -> string = function
    | `Status -> "Status"
    | `Favourite -> "Favourite"
    | `Follow -> "Follow"
    | `Mention -> "Mention"

  let activity_type_t_of_string : string -> activity_type_t = function
    | "Status" -> `Status
    | "Favourite" -> `Favourite
    | "Follow" -> `Follow
    | "Mention" -> `Mention
    | _ -> failwith "activity_type_t_of_string: invalid input"

  type typ_t = [ `reblog | `favourite | `follow | `mention ]

  let typ_t_to_string : typ_t -> string = function
    | `reblog -> "reblog"
    | `favourite -> "favourite"
    | `follow -> "follow"
    | `mention -> "mention"

  let typ_t_of_string : string -> typ_t = function
    | "reblog" -> `reblog
    | "favourite" -> `favourite
    | "follow" -> `follow
    | "mention" -> `mention
    | _ -> failwith "type_t_of_string: invalid input"

  class type t =
    object
      val activity_id : int
      val activity_type : activity_type_t
      val account_id : Account.ID.t
      val from_account_id : Account.ID.t
      val typ : typ_t option [@@column "type"]

      val target_status : Status.t option
      [@@not_column] [@@preload_spec: Status.preload_spec]
    end
end

and Mention = struct
  name "mentions"

  class type t =
    object
      val status_id : Status.ID.t option
      val account_id : Account.ID.t option
    end
end

and Marker = struct
  name "markers"

  class type t =
    object
      val user_id : User.ID.t option
      val timeline : string
      val last_read_id : int
    end
end

and WebPushSubscription = struct
  name "web_push_subscriptions"

  class type t =
    object
      val endpoint : string
      val key_p256dh : string
      val key_auth : string
      val access_token_id : OAuthAccessToken.ID.t option
      val user_id : User.ID.t option
    end
end

and PreviewCard = struct
  name "preview_cards"

  class type t =
    object
      val url : string
      val title : string
      val description : string
      val image_url : string option
      val type_ : int [@@column "type"]
      val html : string
      val author_name : string
      val author_url : string
      val provider_name : string
      val provider_url : string
      val width : int
      val height : int
      val embed_url : string
    end
end

and PreviewCardStatus = struct
  name "preview_cards_statuses"

  class type t =
    object
      val preview_card_id : PreviewCard.ID.t
      val status_id : Status.ID.t
    end
end]
