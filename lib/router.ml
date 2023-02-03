open Util [@@warning "-33"]
open Httpq.Server

let cors =
  Cors.
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
      scope "/api/v1" Api_v1.[
        get "/streaming" Streaming.get;
        get "/instance" Instance.get;
        post "/statuses" Statuses.post;
        scope "/accounts" Accounts.[
          post "/:id/follow" Follow.post;
          post "/:id/unfollow" Unfollow.post;
          get "/search" Search.get;
          get "/verify_credentials" Verify_credentials.get;
        ];
        scope "/timelines" Timelines.[
          get "/home" Home.get;
        ];
        scope "/apps" Apps.[
          post "" Root.post;
          get "/verify_credentials" Verify_credentials.get;
        ];
      ];
      scope "/oauth" Oauth.[
        get "/authorize" Authorize.get;
        post "/authorize" Authorize.post;
        post "/token" Token.post;
      ];
    ] [@ocamlformat "disable"]

let handler =
  middleware_logger @@ middleware_cors cors
  @@ router (routes_from_servers @ routes_from_clients) default_handler
