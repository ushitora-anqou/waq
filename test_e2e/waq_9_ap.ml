open Common

let f =
  make_waq_scenario @@ fun _token ->
  let%lwt r = fetch_exn (waq "/users/user1/outbox") in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  assert (List.assoc "type" l |> expect_string = "OrderedCollection");
  assert (List.assoc "totalItems" l |> expect_int = 0);
  assert (
    List.assoc "first" l |> expect_string = waq "/users/user1/outbox?page=true");
  assert (
    List.assoc "last" l |> expect_string
    = waq "/users/user1/outbox?min_id=0&page=true");

  let%lwt r = fetch_exn (waq "/users/user1/following") in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  assert (List.assoc "type" l |> expect_string = "OrderedCollection");
  assert (List.assoc "totalItems" l |> expect_int = 0);
  assert (
    List.assoc "first" l |> expect_string = waq "/users/user1/following?page=1");

  let%lwt r = fetch_exn (waq "/users/user1/followers") in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  assert (List.assoc "type" l |> expect_string = "OrderedCollection");
  assert (List.assoc "totalItems" l |> expect_int = 0);
  assert (
    List.assoc "first" l |> expect_string = waq "/users/user1/followers?page=1");

  Lwt.return_unit
