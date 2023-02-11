open Common

let f =
  make_waq_scenario @@ fun _token ->
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
  let account_id = l |> List.assoc "id" |> expect_string in

  let%lwt r = fetch_exn (waq "/api/v1/instance") in
  let l = Yojson.Safe.from_string r |> expect_assoc in
  assert (l |> List.mem_assoc "uri");

  let%lwt r = get_account `Waq account_id in
  assert (r.id = account_id);
  assert (r.username = "user1");
  assert (r.acct = "user1");
  assert (r.last_status_at = None);
  assert (r.statuses_count = 0);
  assert (r.followers_count = 0);
  assert (r.following_count = 0);

  Lwt.return_unit
