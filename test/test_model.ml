open Waq.Model

let test_sort_statuses_by_dfs () =
  let make_status ~id ?in_reply_to_id () =
    Status.make ~spoiler_text:"" ~account_id:(Account.ID.of_int 0) ~text:""
      ~uri:"" ~id:(Status.ID.of_int id)
      ?in_reply_to_id:(in_reply_to_id |> Option.map Status.ID.of_int)
      ~visibility:`Public ()
  in
  let s0 = make_status ~id:0 () in
  let s1 = make_status ~id:1 ~in_reply_to_id:0 () in
  let s2 = make_status ~id:2 ~in_reply_to_id:1 () in
  assert (Status.sort_statuses_by_dfs s0#id [ s0 ] = [ s0 ]);
  assert (Status.sort_statuses_by_dfs s1#id [ s2 ] = [ s2 ]);

  ()

let () =
  let open Alcotest in
  run "model"
    [
      ( "sort_statuses_by_dfs",
        [ test_case "case1" `Quick test_sort_statuses_by_dfs ] );
    ]
