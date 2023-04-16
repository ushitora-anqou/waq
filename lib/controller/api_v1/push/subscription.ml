open Helper
open Lwt.Infix
open Util

let expect_assoc = function `Assoc l -> l | _ -> failwith "expect assoc"
let expect_string = function `String s -> s | _ -> failwith "expect string"

let get req =
  let%lwt oauth_access_token = authenticate_bearer req in
  match%lwt
    Db.(
      e
        WebPushSubscription.(
          get_one ~access_token_id:(Some oauth_access_token#id))
      |> maybe_no_row)
  with
  | None -> raise_error_response `Not_found
  | Some s ->
      s |> Entity.serialize_web_push_subscription
      |> Entity.yojson_of_web_push_subscription |> respond_yojson

let post req =
  let%lwt oauth_access_token = authenticate_bearer req in

  let%lwt body = Httpq.Server.body req in
  let endpoint, key_p256dh, key_auth =
    try
      let subscription =
        Yojson.Safe.from_string body
        |> expect_assoc |> List.assoc "subscription" |> expect_assoc
      in
      let keys = List.assoc "keys" subscription |> expect_assoc in
      ( List.assoc "endpoint" subscription |> expect_string,
        List.assoc "p256dh" keys |> expect_string,
        List.assoc "auth" keys |> expect_string )
    with _ -> raise_error_response `Bad_request
  in

  Db.(
    e
      WebPushSubscription.(
        make ~endpoint ~key_p256dh ~key_auth
          ~access_token_id:oauth_access_token#id
          ?user_id:oauth_access_token#resource_owner_id ()
        |> save_one))
  >|= Entity.(
        serialize_web_push_subscription *> yojson_of_web_push_subscription)
  >>= respond_yojson

let delete req =
  let open Model.WebPushSubscription in
  let%lwt oauth_access_token = authenticate_bearer req in
  (try%lwt
     let%lwt s =
       Db.(e @@ get_one ~access_token_id:(Some oauth_access_token#id))
     in
     Db.(e @@ delete [ s ])
   with Sqlx.Error.NoRowFound -> Lwt.return_unit);%lwt
  respond_yojson (`Assoc [])
