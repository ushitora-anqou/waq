open Waq

let test_mention_regex () =
  let m = Controller.Api_v1.Statuses.Root.match_mention in
  assert (m "test @foo" = [ ("foo", None) ]);
  assert (m "test @foo@example.com" = [ ("foo", Some "example.com") ]);
  assert (m "test@foo" = []);
  assert (m "\\@foo" = []);
  assert (m "@foo@example\\.com" = []);
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
