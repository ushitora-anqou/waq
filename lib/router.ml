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
  let open Router in
  Controller.
    [
      get "/.well-known/host-meta" Well_known_host_meta.get;
      get "/.well-known/webfinger" Well_known_webfinger.get;
      scope "/users/:name"
        Users.
          [
            get "" Root.get;
            post "/inbox" Inbox.post;
            get "/statuses/:id" Statuses.get;
          ];
    ]

let routes_from_clients =
  let open Router in
  Controller.
    [
      scope "/api/v1" Api_v1.[
        get "/streaming" Streaming.get;
        get "/instance" Instance.get;
        scope "/statuses" Statuses.[
          post "" Root.post;
          get "/:id" Root.get;
          get "/:id/context" Context.get;
          post "/:id/reblog" Reblog.post;
          post "/:id/favourite" Favourite.post;
          post "/:id/unfavourite" Unfavourite.post;
          get "/:id/favourited_by" Favourited_by.get;
        ];
        scope "/accounts" Accounts.[
          post "/:id/follow" Follow.post;
          post "/:id/unfollow" Unfollow.post;
          get "/search" Search.get;
          get "/verify_credentials" Verify_credentials.get;
          get "/lookup" Lookup.get;
        ];
        scope "/timelines" Timelines.[
          get "/home" Home.get;
        ];
        scope "/apps" Apps.[
          post "" Root.post;
          get "/verify_credentials" Verify_credentials.get;
        ];
      ];
      scope "/api/v2" Api_v2.[
        get "/search" Search.get;
      ];
      scope "/oauth" Oauth.[
        get "/authorize" Authorize.get;
        post "/authorize" Authorize.post;
        post "/token" Token.post;
      ];
    ] [@ocamlformat "disable"]

let handler =
  Logger.use @@ Cors.use cors
  @@ Router.use (routes_from_servers @ routes_from_clients) default_handler
