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
  Controller.
    [
      get "/.well-known/host-meta" Well_known_host_meta.get;
      get "/.well-known/webfinger" Well_known_webfinger.get;
      get "/users/:name" Users.get;
      post "/users/:name/inbox" Inbox.post;
    ]

let routes_from_clients =
  Controller.
    [
      post "/api/v1/accounts/:id/follow" Api_v1_accounts_follow.post;
      post "/api/v1/accounts/:id/unfollow" Api_v1_accounts_unfollow.post;
      get "/api/v1/accounts/search" Api_v1_accounts_search.get;
      post "/api/v1/statuses" Api_v1_statuses.post;
      get "/api/v1/timelines/home" Api_v1_timelines_home.get;
      post "/api/v1/apps" Api_v1_apps.post;
      get "/oauth/authorize" Oauth_authorize.get;
      post "/oauth/authorize" Oauth_authorize.post;
      post "/oauth/token" Oauth_token.post;
      get "/api/v1/apps/verify_credentials" Api_v1_apps_verify_credentials.get;
      get "/api/v1/streaming" Api_v1_streaming.get;
      get "/api/v1/accounts/verify_credentials"
        Api_v1_accounts_verify_credentials.get;
      get "/api/v1/instance" Api_v1_instance.get;
    ]

let handler =
  let open Httpq.Server in
  middleware_logger @@ middleware_cors cors
  @@ router (routes_from_servers @ routes_from_clients) default_handler
