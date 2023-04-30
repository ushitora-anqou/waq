open Waq

let test_match_mention () =
  let m = Text_helper.match_mention in
  assert (m "test @foo" = [ (5, 4, "foo", None) ]);
  assert (m "test @foo@example.com" = [ (5, 16, "foo", Some "example.com") ]);
  assert (m "test@foo" = []);
  assert (m "\\@foo" = []);
  assert (m "https://example.com/@foo" = []);
  assert (m "@foo@example\\.com" = []);
  assert (
    m "@foo@example.com:3000" = [ (0, 21, "foo", Some "example.com:3000") ]);
  assert (m "@foo @bar test" = [ (0, 4, "foo", None); (5, 4, "bar", None) ]);
  assert (
    m "@foo@example.com @bar@example.com test"
    = [
        (0, 16, "foo", Some "example.com"); (17, 16, "bar", Some "example.com");
      ]);
  assert (
    m "@admin@localhost:3000 @anqou@pleroma-dev-e8f73c6cd1b6.anqou.net てすと"
    = [
        (0, 21, "admin", Some "localhost:3000");
        (22, 41, "anqou", Some "pleroma-dev-e8f73c6cd1b6.anqou.net");
      ]);
  assert (
    m "@f-_oo@exa-mple.comてすと@bar-_@waq.exa_mple.com"
    = [
        (0, 19, "f-_oo", Some "exa-mple.com");
        (28, 23, "bar-_", Some "waq.exa_mple.com");
      ]);
  assert (m "てすと@f-_てすと" = [ (9, 4, "f-_", None) ]);
  assert (m "てすと @f-_てすと" = [ (10, 4, "f-_", None) ]);
  ()

let test_substitute_with_match_mention () =
  let r spec =
    Text_helper.substitute
      (spec
      |> List.map (fun (off, len, subtext) ->
             Text_helper.make_subst ~off ~len ~subtext))
  in
  let m = Text_helper.match_mention in
  assert (
    let text =
      "@admin@localhost:3000 @anqou@pleroma-dev-e8f73c6cd1b6.anqou.net てすと"
    in
    r (m text |> List.map (fun (off, len, n, _) -> (off, len, "@" ^ n))) text
    = "@admin @anqou てすと");
  ()

let test_substitute () =
  let r spec =
    Text_helper.substitute
      (spec
      |> List.map (fun (off, len, subtext) ->
             Text_helper.make_subst ~off ~len ~subtext))
  in
  assert (r [] "foo" = "foo");
  assert (r [ (0, 1, "bar") ] "foo" = "baroo");
  assert (r [ (0, 1, "bar"); (1, 3, "baz") ] "foooo" = "barbazo");
  assert (
    r [ (0, 2, "foo"); (1, 3, "bar"); (0, 3, "baz"); (3, 1, "hoge") ] "abcde"
    = "foochogee");
  ()

let () =
  let open Alcotest in
  run "text_helper"
    [
      ( "mention",
        [
          test_case "match" `Quick test_match_mention;
          test_case "substitute with match_mention" `Quick
            test_substitute_with_match_mention;
        ] );
      ("substitute", [ test_case "case1" `Quick test_substitute ]);
    ]
