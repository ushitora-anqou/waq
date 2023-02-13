module Uri = Httpq.Uri
module Ptime = Waq.Util.Ptime

let all_tests =
  [
    ("waq-mstdn-1", Waq_mstdn_1.f);
    ("waq-mstdn-2", Waq_mstdn_2.f);
    ("waq-mstdn-3", Waq_mstdn_3_reply.f);
    ("waq-mstdn-4", Waq_mstdn_4_reblog.f);
    ("waq-mstdn-5", Waq_mstdn_5_reblog.f);
    ("waq-mstdn-6", Waq_mstdn_6_fav.f);
    ("waq-mstdn-7", Waq_mstdn_7_fav.f);
    ("waq-mstdn-8", Waq_mstdn_8_lookup_search.f);
    ("waq-1", Waq_1.f);
    ("waq-2", Waq_2_ws.f);
    ("waq-3", Waq_3.f);
    ("waq-4", Waq_4_reblog.f);
    ("waq-5", Waq_5_fav.f);
    ("waq-6", Waq_6_rel.f);
  ]

let execute_one_test (name, f) =
  Logq.debug (fun m -> m "===== Testcase %s =====" name);
  f ()

let () =
  print_newline ();
  Logq.(add_reporter (make_reporter ~l:Debug ()));

  let shuffle d =
    (* Thanks to: https://stackoverflow.com/a/15095713 *)
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond
  in
  let chosen_tests =
    match Sys.argv with
    | [| _ |] -> shuffle all_tests
    | _ ->
        Sys.argv |> Array.to_list |> List.tl
        |> List.map (fun name -> (name, List.assoc name all_tests))
  in

  Logq.info (fun m ->
      chosen_tests |> List.map fst |> String.concat " "
      |> m "[e2e] Chosen tests: %s");

  chosen_tests |> List.iter execute_one_test
