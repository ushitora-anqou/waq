open Util [@@warning "-33"]

let get = Httpq.Server.get
let post = Httpq.Server.post

let cors =
  Httpq.Server.Cors.
    [
      make "/.well-known/*" ~methods:[ `GET ] ();
      make "/users/:username" ~methods:[ `GET ] ();
      make "/api/*" ~methods:[ `POST; `PUT; `DELETE; `GET; `PATCH; `OPTIONS ] ();
      make "/oauth/token" ~methods:[ `POST ] ();
    ]

let routes_from_servers =
  [
    get "/.well-known/host-meta" Controller.Well_known_host_meta.get;
    get "/.well-known/webfinger" Controller.Well_known_webfinger.get;
    get "/users/:name" Controller.Users.get;
    post "/users/:name/inbox" Controller.Inbox.post;
  ]

let routes_from_clients =
  [
    post "/api/v1/accounts/:id/follow" Controller.Api_v1_accounts_follow.post;
    post "/api/v1/accounts/:id/unfollow"
      Controller.Api_v1_accounts_unfollow.post;
    get "/api/v1/accounts/search" Controller.Api_v1_accounts_search.get;
    post "/api/v1/statuses" Controller.Api_v1_statuses.post;
    get "/api/v1/timelines/home" Controller.Api_v1_timelines_home.get;
    post "/api/v1/apps" Controller.Api_v1_apps.post;
    get "/oauth/authorize" Controller.Oauth_authorize.get;
    post "/oauth/authorize" Controller.Oauth_authorize.post;
    post "/oauth/token" Controller.Oauth_token.post;
    get "/api/v1/apps/verify_credentials"
      Controller.Api_v1_apps_verify_credentials.get;
    get "/api/v1/streaming" Controller.Api_v1_streaming.get;
    get "/api/v1/accounts/verify_credentials"
      Controller.Api_v1_accounts_verify_credentials.get;
    get "/api/v1/instance" Controller.Api_v1_instance.get;
  ]

let handler =
  let open Httpq.Server in
  middleware_logger @@ middleware_cors cors
  @@ router (routes_from_servers @ routes_from_clients) default_handler
