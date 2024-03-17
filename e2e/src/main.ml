module Uri = Httpq.Uri
module Ptime = Waq.Util.Ptime

let all_tests =
  [
    ("waq-mstdn-1", Waq_mstdn_1.f);
    (*
    ("waq-mstdn-2", Waq_mstdn_2.f);
    ("waq-mstdn-3", Waq_mstdn_3_reply.f);
    ("waq-mstdn-4", Waq_mstdn_4_reblog.f);
    ("waq-mstdn-5", Waq_mstdn_5_reblog.f);
    ("waq-mstdn-6", Waq_mstdn_6_fav.f);
    ("waq-mstdn-7", Waq_mstdn_7_fav.f);
    ("waq-mstdn-8", Waq_mstdn_8_lookup_search.f);
    ("waq-mstdn-9-1", Waq_mstdn_9_delete.f_waq_mstdn);
    ("waq-mstdn-9-2", Waq_mstdn_9_delete.f_mstdn_waq);
    ("waq-mstdn-10-1", Waq_mstdn_10_attachment.f_waq_mstdn);
    ("waq-mstdn-10-2", Waq_mstdn_10_attachment.f_mstdn_waq);
    ("waq-mstdn-10-3", Waq_mstdn_10_attachment.f_waq_waq);
    ("waq-mstdn-13", Waq_mstdn_13_v2_search.f);
    *)
    ("waq-1", Waq_1.f);
    ("waq-2", Waq_2_ws.f);
    ("waq-3", Waq_3.f);
    ("waq-4", Waq_4_reblog.f);
    ("waq-5", Waq_5_fav.f);
    ("waq-6", Waq_6_rel.f);
    ("waq-7", Waq_7_reblog.f);
    ("waq-8", Waq_8_delete.f);
    ("waq-9", Waq_9_ap.f);
    ("waq-10", Waq_10_mention.f);
    ("waq-11", Waq_11_marker.f);
    (*
    (* ********** *)
    ("01-mention-waq-mstdn-waq", Waq_mstdn_11_mention.f_waq_mstdn_waq);
    ("01-mention-mstdn-waq-waq", Waq_mstdn_11_mention.f_mstdn_waq_waq);
    ("01-mention-waq-waq-waq", Waq_mstdn_11_mention.f_waq_waq_waq);
    ("01-mention-mstdn-waq-mstdn", Waq_mstdn_11_mention.f_mstdn_waq_mstdn);
    ("02-summary-mstdn-waq", Waq_mstdn_12_summary.f_mstdn_waq);
    ("02-summary-waq-mstdn", Waq_mstdn_12_summary.f_waq_mstdn);
    ("02-summary-waq-waq", Waq_mstdn_12_summary.f_waq_waq);
    ("03-preview-card-waq-waq", Waq_mstdn_14_preview_card.f_waq_waq);
    ("03-preview-card-mstdn-waq", Waq_mstdn_14_preview_card.f_mstdn_waq);
    ("04-text-waq-waq", Waq_mstdn_15_text.f_waq_waq);
    ("04-text-waq-mstdn", Waq_mstdn_15_text.f_waq_mstdn);
    ("04-text-mstdn-waq", Waq_mstdn_15_text.f_mstdn_waq);
    ("06-cred-waq-waq", Waq_mstdn_16_cred.f_waq_waq);
    ("06-cred-waq-mstdn", Waq_mstdn_16_cred.f_waq_mstdn);
    ("06-cred-mstdn-waq", Waq_mstdn_16_cred.f_mstdn_waq);
    *)
  ]

let execute_one_test (name, f) =
  Logq.debug (fun m -> m "===== Testcase %s =====" name);
  try f ()
  with e ->
    Logq.err (fun m -> m "!!!!! !!!!! !!!!! !!!!!");
    Logq.err (fun m -> m "Testcase %s failed: %s" name (Printexc.to_string e));
    Logq.err (fun m -> m "!!!!! !!!!! !!!!! !!!!!");
    raise e

let () =
  print_newline ();
  Logq.(add_reporter (make_reporter ~l:Debug ()));
  Random.self_init ();

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
