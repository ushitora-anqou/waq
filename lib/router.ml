open Util [@@warning "-33"]
open Httpq.Server

let cors =
  Cors.
    [
      make "/nodeinfo/2.0" ~methods:[ `GET ] ();
      make "/.well-known/*" ~methods:[ `GET ] ();
      make "/users/:username" ~methods:[ `GET ] ();
      make "/api/*" ~methods:[ `POST; `PUT; `DELETE; `GET; `PATCH; `OPTIONS ] ();
      make "/oauth/token" ~methods:[ `POST ] ();
    ]

let routes =
  let open Router in
  Controller.
    [
      get "/" Root.get;
      get "/system/*" Static.get;
      post "/inbox" Inbox.post;
      get "/nodeinfo/2.0" Nodeinfo.get_2_0;
      scope "/.well-known" Well_known.[
        get "/host-meta" Host_meta.get;
        get "/webfinger" Webfinger.get;
        get "/nodeinfo" Nodeinfo.get;
      ];
      scope "/users/:name" Users.[
        get "" Root.get;
        post "/inbox" Inbox.post;
        get "/outbox" Outbox.get;
        get "/followers" Following.get_followers;
        get "/following" Following.get_following;
        get "/statuses/:id" Statuses.get;
      ];
      scope "/api/v1" Api_v1.[
        get "/streaming" Streaming.get;
        get "/instance" Instance.get;
        scope "/notifications" Notifications.[
          get "" Root.get;
        ];
        scope "/statuses" Statuses.[
          post "" Root.post;
          get "/:id" Root.get;
          delete "/:id" Root.delete;
          get "/:id/context" Context.get;
          post "/:id/reblog" Reblog.post;
          post "/:id/unreblog" Unreblog.post;
          post "/:id/favourite" Favourite.post;
          post "/:id/unfavourite" Unfavourite.post;
          get "/:id/favourited_by" Favourited_by.get;
        ];
        scope "/accounts" Accounts.[
          get "/:id/statuses" Statuses.get;
          post "/:id/follow" Follow.post;
          post "/:id/unfollow" Unfollow.post;
          get "/:id/followers" Following.get_followers;
          get "/:id/following" Following.get_following;
          get "/search" Search.get;
          get "/verify_credentials" Verify_credentials.get;
          get "/lookup" Lookup.get;
          get "/relationships" Relationships.get;
          patch "/update_credentials" Update_credentials.patch;
          get "/:id" Root.get; (* Wildcard match *)
        ];
        scope "/timelines" Timelines.[
          get "/home" Home.get;
        ];
        scope "/apps" Apps.[
          post "" Root.post;
          get "/verify_credentials" Verify_credentials.get;
        ];
        scope "/push" Push.[
          get "/subscription" Subscription.get;
          post "/subscription" Subscription.post;
          delete "/subscription" Subscription.delete;
        ];
        get "/markers" Markers.get;
        post "/markers" Markers.post;
        post "/media" Media.post;
      ];
      scope "/api/v2" Api_v2.[
        get "/search" Search.get;
        post "/media" Media.post;
      ];
      scope "/oauth" Oauth.[
        get "/authorize" Authorize.get;
        post "/authorize" Authorize.post;
        post "/token" Token.post;
      ];
    ] [@ocamlformat "disable"]

let handler =
  Logger.use ?dump_req_dir:(Config.debug_dump_req_dir ())
  @@ Cors.use cors
  @@ Router.use routes default_handler
