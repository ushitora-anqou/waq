open Waq
open Regex

let test_replace () =
  let r = replace in
  assert (
    "foo bar baz baz" |> r (e "baz") (fun _ -> "2000") = "foo bar 2000 2000");
  ()

let test_match () =
  let m = match_ in
  assert (
    m (e "a") "a" = [ [| Some { offset = 0; length = 1; substr = "a" } |] ]);
  assert (
    m (e "a(b)") "ab"
    = [
        [|
          Some { offset = 0; length = 2; substr = "ab" };
          Some { offset = 1; length = 1; substr = "b" };
        |];
      ]);
  ()

let () =
  let open Alcotest in
  run "regex"
    [
      ("replace", [ test_case "case1" `Quick test_replace ]);
      ("match", [ test_case "case1" `Quick test_match ]);
    ]
