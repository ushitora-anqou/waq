open Common

let f =
  make_waq_scenario @@ fun _waq_token ->
  let%lwt access_token = fetch_access_token ~username:"user1" in

  let%lwt r =
    fetch_exn
      ~headers:[ (`Authorization, "Bearer " ^ access_token) ]
      (waq "/api/v1/apps/verify_credentials")
  in
  assert (
    match Yojson.Safe.from_string r with
    | `Assoc l -> l |> List.assoc "name" |> expect_string = "foo"
    | _ -> false);

  let%lwt r =
    fetch_exn
      ~headers:[ (`Authorization, "Bearer " ^ access_token) ]
      (waq "/api/v1/accounts/verify_credentials")
  in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  assert (l |> List.assoc "username" |> expect_string = "user1");
  assert (l |> List.assoc "acct" |> expect_string = "user1");
  assert (
    l |> List.assoc "source" |> expect_assoc |> List.assoc "privacy"
    |> expect_string = "public");

  let%lwt r = fetch_exn (waq "/api/v1/instance") in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  assert (l |> List.mem_assoc "uri");

  Lwt.return_unit
