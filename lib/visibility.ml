type t = Db.Status.visibility_type_t

let to_string ?(limited = "private") = function
  | `Public -> "public"
  | `Unlisted -> "unlisted"
  | `Private -> "private"
  | `Direct -> "direct"
  | `Limited -> limited

let of_string_opt = function
  | "public" -> Some `Public
  | "unlisted" -> Some `Unlisted
  | "private" -> Some `Private
  | "direct" -> Some `Direct
  | _ -> None

let is_visible ?(account : Db.Account.t option (* preload: following *))
    (status : Db.Status.t (* preload: mentions *)) =
  match status#visibility with
  | `Public | `Unlisted -> true
  | (`Private | `Direct) when Option.is_some account ->
      let is_self = status#account_id = (Option.get account)#id in
      let has_mention =
        status#mentions
        |> List.exists (fun (m : Db.Mention.t) ->
               m#account_id = Some (Option.get account)#id)
      in
      let is_following =
        (Option.get account)#following
        |> List.exists (fun f -> f#target_account_id = status#account_id)
      in
      is_self || has_mention || (status#visibility = `Private && is_following)
  | _ -> false
