open Waq.Util

let test_iota () = assert (iota 10 = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ])

let test_take () =
  assert (List.take 0 [ 0; 1; 2; 3 ] = []);
  assert (List.take 1 [ 0; 1; 2; 3 ] = [ 0 ]);
  assert (List.take 2 [ 0; 1; 2; 3; 4; 5 ] = [ 0; 1 ])

let test_lwt_list_partition_map_p () =
  let left, right =
    Lwt_main.run
      ([ 1; 2; 3; 4; 5 ]
      |> Lwt_list.partition_map_p (fun x ->
             if x mod 2 = 0 then Either.Left x |> Lwt.return
             else Either.Right x |> Lwt.return))
  in
  assert (left = [ 2; 4 ]);
  assert (right = [ 1; 3; 5 ]);
  ()

let () =
  let open Alcotest in
  run "util"
    [
      ("iota", [ test_case "case1" `Quick test_iota ]);
      ("take", [ test_case "case1" `Quick test_take ]);
      ( "lwt_list_partition_map_p",
        [ test_case "case1" `Quick test_lwt_list_partition_map_p ] );
    ]
