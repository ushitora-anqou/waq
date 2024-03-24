open Yume

let test_parse_path () =
  let open Path_pattern in
  assert (of_string "" = []);
  assert (of_string "/foo/bar/2000" = [ L "foo"; L "bar"; L "2000" ]);
  assert (of_string "/foo/:bar/2000" = [ L "foo"; P ":bar"; L "2000" ]);
  assert (of_string "/foo/*" = [ L "foo"; S ]);
  assert (of_string "/foo/:bar" = [ L "foo"; P ":bar" ]);
  assert (perform ~pat:(of_string "/foo/:bar") "/foo/1" = Some [ (":bar", "1") ]);
  assert (perform ~pat:(of_string "/foo/*") "/foo/1/2" |> Option.is_some);
  assert (perform ~pat:(of_string "/foo/bar") "/foo/bar/" |> Option.is_some);
  assert (perform ~pat:(of_string "/foo/bar") "/foo/bar//" |> Option.is_some);
  ()

let () =
  let open Alcotest in
  run "path_pattern"
    [ ("parse_path", [ test_case "case1" `Quick test_parse_path ]) ]
