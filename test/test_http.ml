open Waq
open Http

let test_parse_path () =
  assert (parse_path "" = []);
  assert (parse_path "/foo/bar/2000" = [ `L "foo"; `L "bar"; `L "2000" ]);
  assert (parse_path "/foo/:bar/2000" = [ `L "foo"; `P ":bar"; `L "2000" ]);
  ()

let () =
  let open Alcotest in
  run "http" [ ("parse_path", [ test_case "case1" `Quick test_parse_path ]) ]
