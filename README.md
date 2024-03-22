# Waq

Waq is yet another ActivityPub server implementation written in OCaml.

- Waq provides a microblogging service and has some basic functionality such as posting, reblogging, favourites, mentions, notifications, and so on. 
- Waq provides REST and Websocket streaming APIs that are compatible with Mastodon. You can use your favourite Mastodon client such as [Elk](https://elk.zone).
- Waq acts as an ActivityPub server and can interact with other servers like Mastodon and Pleroma to share posts and favourites.

A blog post about Waq is [here](https://hackmd.io/@anqou/H1qRfp_Fn). Its original Japanese version is [there](https://hackmd.io/@anqou/rka_GANYh).

## Note for those trying to deploy Waq

Although Waq can be deployed and used well for your daily microblogging, it currently lacks many of the features you would expect from a standard SNS, such as post privacy, custom emojis, profile editing, and so on. At the moment, I would not recommend using Waq as your primary SNS.

## Quick start

Install docker beforehand. Then:
```
cd e2e
make start-ngrok
make create-cluster
make start-waq
make start-mastodon
make start-elk
make waq-port-forward &
make mastodon-port-forward &
make elk-port-forward &
cat _test_waq # Access this domain for Waq.
cat _test_mastodon # Access this domain for Mastodon.
cat _test_elk # Access this domain for Elk.
```

Create a demo user:
```
kubectl exec -n e2e deploy/waq-web -- /waq/waq user:register --username=demo --password=demo --display-name=demo --email=demo@example.com
```

When shutting down:
```
make clean-cluster
make stop-ngrok
```

## How to run E2E tests

```
cd e2e
make start-ngrok && make create-cluster && make test
```

## Technology stack

The OCaml libraries that Waq depends on (The full list is [here](https://github.com/ushitora-anqou/waq/blob/master/dune-project)).

- RDBMS: PostgreSQL
- HTTP server: [ocaml-cohttp](https://github.com/mirage/ocaml-cohttp)
  - [Original Web framework](https://github.com/ushitora-anqou/waq/tree/master/lib_httpq) (strongly inspired by [Dream](https://github.com/aantron/dream)) is written on top of this library.
- Websocket: [ocaml-websocket](https://github.com/vbmithr/ocaml-websocket)
- RDBMS driver: [postgresql-ocaml](https://github.com/mmottl/postgresql-ocaml)
  - [Original O/R mapper](https://github.com/ushitora-anqou/waq/tree/master/lib_sqlx) is written on top of this library.
- Concurrent I/O: [lwt](https://github.com/ocsigen/lwt)
- HTML template engine: [jingoo](https://github.com/tategakibunko/jingoo)

### Original O/R mapper (`lib_sqlx`)

To make it easy to construct typical SQL queries and handle their results,
Waq has an original O/R mapper library `lib_sqlx`. It has a PPX driver that allows us
to write SQL schemas as OCaml code (see [lib/schema.ml](https://github.com/ushitora-anqou/waq/blob/master/lib/schema.ml) for an example).

An example usage of `lib_sqlx` (working code is [here](https://github.com/ushitora-anqou/waq/blob/master/lib_sqlx/test/test_example.ml)):

```ocaml
open Sqlx

[%%sqlx.schemas
(* Define two RDB schemas (accounts and statuses) *)
[%%sqlx.schemas
module rec Account = struct
  name "accounts"

  class type t =
    object
      (* Table `accounts` has 3 columns ... *)
      val username : string
      val domain : string option
      val display_name : string

      (* ... and also has many `statuses` thorugh foreign key *)
      val statuses : Status.t list [@@foreign_key `account_id]
    end
end

and Status = struct
  name "statuses"

  class type t =
    object
      (* Table `statuses` has 4 columns *)
      val text : string
      val in_reply_to_id : ID.t option
      val reblog_of_id : ID.t option
      val account_id : Account.ID.t
    end
end]

module Db = struct
  include Engine.Make (Driver_pg)
end

(**)

(* Insert some records into table `statuses` for testing *)
let insert_some_statuses () : unit Lwt.t =
  (* Insert two accounts (`a1` and `a2`) *)
  let%lwt a1 =
    Db.e Account.(make ~username:"user1" ~display_name:"User 1" () |> save_one)
  in
  let%lwt a2 =
    Db.e
      Account.(
        make ~username:"user2" ~domain:"example.com" ~display_name:"User 2" ()
        |> save_one)
  in
  (* Insert two statuses published by the accounts `a1` and `a2`. The second status is a reply to the first one.  *)
  let%lwt s1 =
    Db.e Status.(make ~account_id:a1#id ~text:"Hello" () |> save_one)
  in
  let%lwt _ =
    Db.e
      Status.(
        make ~account_id:a2#id ~text:"World" ~in_reply_to_id:s1#id ()
        |> save_one)
  in
  Lwt.return_unit
  [@@warning "-8"]

(* Select all statuses that `username` has replied *)
let statuses_replied_by ~(username : string) : Status.t list Lwt.t =
  let%lwt acct =
    Db.e Account.(get_one ~username ~preload:[ `statuses [ `in_reply_to [] ] ])
  in
  acct#statuses |> List.filter_map (fun s -> s#in_reply_to) |> Lwt.return
```

## License

MIT
