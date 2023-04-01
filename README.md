# Waq

Waq is yet another ActivityPub server implementation written in OCaml.

- Waq provides a microblogging service and has some basic functionality such as posting, reblogging, favourites, mentions, notifications, and so on. 
- Waq provides REST and Websocket streaming APIs that are compatible with Mastodon. You can use your favourite Mastodon client such as [Elk](https://elk.zone).
- Waq acts as an ActivityPub server and can interact with other servers like Mastodon and Pleroma to share posts and favourites.

## Note for those trying to deploy Waq

Although Waq can be deployed and used well for your daily microblogging, it currently lacks many of the features you would expect from a standard SNS, such as post privacy, custom emojis, profile editing, and so on. At the moment, I would not recommend using Waq as your primary SNS.

## Deploy with Docker

The easiest way to deploy Waq is to use `docker compose`.

```
$ git clone https://github.com/ushitora-anqou/waq
$ cd waq/docker
$ vim config/prod.yml # Change `server_name`
$ docker compose pull
$ docker compose run waq /root/waq db:migrate
$ docker compose run waq /root/waq user:register
$ docker compose up -d
```

When you update Waq:

```
$ cd waq/docker
$ docker compose pull
$ docker compose run waq /root/waq db:migrate
$ docker compose down && docker compose up -d
```

## Build and test

```
$ git clone https://github.com/ushitora-anqou/waq
$ cd waq
$ opam switch create . 4.14.1 --no-install
$ opam install . --deps-only
$ dune build && dune runtest
```

FIXME: Write about E2E test

## Technology stack

The OCaml libraries that Waq depends on are listed [here](https://github.com/ushitora-anqou/waq/blob/master/dune-project).

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
[%%sqlx.schemas
module rec Account = struct
  name "accounts"

  class type t =
    object
      val username : string
      val domain : string option
      val display_name : string
      val statuses : Status.t list [@@foreign_key `account_id]
    end
end

and Status = struct
  name "statuses"

  class type t =
    object
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

let insert_some_statuses () : unit Lwt.t =
  let%lwt a =
    Db.e
      Account.(
        make ~username:"anqou" ~domain:"example.com" ~display_name:"Anqou" ()
        |> save_one)
  in
  let%lwt s1 =
    Db.e Status.(make ~account_id:a#id ~text:"Hello" () |> save_one)
  in
  let%lwt _ =
    Db.e
      Status.(
        make ~account_id:a#id ~text:"World" ~in_reply_to_id:s1#id () |> save_one)
  in
  Lwt.return_unit
  [@@warning "-8"]

let replied_statuses_by_account ~(username : string) : Status.t list Lwt.t =
  let%lwt acct =
    Db.e Account.(get_one ~username ~preload:[ `statuses [ `in_reply_to [] ] ])
  in
  acct#statuses |> List.filter_map (fun s -> s#in_reply_to) |> Lwt.return
```
