open Waq
open Regex

let test_replace () =
  let r = replace in
  assert (
    "foo bar baz baz" |> r (e "baz") (fun _ -> "2000") = "foo bar 2000 2000");
  ()

let () =
  let open Alcotest in
  run "regex" [ ("replace", [ test_case "case1" `Quick test_replace ]) ]
