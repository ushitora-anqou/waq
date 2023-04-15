open Webpush

let test_main_case1 () =
  (* Thanks to: https://github.com/pimeys/rust-web-push/blob/c5a8e1250b3fc85c5005dcb321e01ba4253b584d/src/vapid/builder.rs#L351-L362 *)
  let endpoint =
    "https://updates.push.services.mozilla.com/wpush/v2/gAAAAABaso4Vajy4STM25r5y5oFfyN451rUmES6mhQngxABxbZB5q_o75WpG25oKdrlrh9KdgWFKdYBc-buLPhvCTqR5KdsK8iCZHQume-ndtZJWKOgJbQ20GjbxHmAT1IAv8AIxTwHO-JTQ2Np2hwkKISp2_KUtpnmwFzglLP7vlCd16hTNJ2I"
  in
  let subscriber = "mailto:example@example.com" in
  let priv_key = "IQ9Ur0ykXoHS9gzfYX0aBjy9lvdrjx_PFUXmie9YRcY" in
  match Vapid.build ~endpoint ~subscriber ~priv_key with
  | Error _ -> assert false
  | Ok r ->
      assert (
        r.k
        = "BMjQIp55pdbU8pfCBKyXcZjlmER_mXt5LqNrN1hrXbdBS5EnhIbMu3Au-RV53iIpztzNXkGI56BFB1udQ8Bq_H4")

let () =
  let open Alcotest in
  run "webpush" [ ("main", [ test_case "case1" `Quick test_main_case1 ]) ]
