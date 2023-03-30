open Waq

let test_mention_regex () =
  let m = Controller.Api_v1.Statuses.Root.match_mention in
  assert (m "test @foo" = [ ("foo", None) ]);
  assert (m "test @foo@example.com" = [ ("foo", Some "example.com") ]);
  assert (m "test@foo" = []);
  assert (m "\\@foo" = []);
  assert (m "https://example.com/@foo" = []);
  assert (m "@foo@example\\.com" = []);
  assert (m "@foo@example.com:3000" = [ ("foo", Some "example.com:3000") ]);
  assert (m "@foo @bar test" = [ ("foo", None); ("bar", None) ]);
  assert (
    m "@foo@example.com @bar@example.com test"
    = [ ("foo", Some "example.com"); ("bar", Some "example.com") ]);
  assert (
    m "@admin@localhost:3000 @anqou@pleroma-dev-e8f73c6cd1b6.anqou.net てすと"
    = [
        ("admin", Some "localhost:3000");
        ("anqou", Some "pleroma-dev-e8f73c6cd1b6.anqou.net");
      ]);
  assert (
    m "@f-_oo@exa-mple.comてすと@bar-_@waq.exa_mple.com"
    = [ ("f-_oo", Some "exa-mple.com"); ("bar-_", Some "waq.exa_mple.com") ]);
  assert (m "てすと@f-_てすと" = [ ("f-_", None) ]);
  assert (m "てすと @f-_てすと" = [ ("f-_", None) ]);
  ()

let () =
  let open Alcotest in
  run "controller"
    [ ("mention", [ test_case "regex" `Quick test_mention_regex ]) ]
