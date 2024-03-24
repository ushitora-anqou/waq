open Helper

let get_2_0 _ _req =
  (* FIXME: Fill in correct values *)
  `Assoc
    [
      ("version", `String "2.0");
      ( "software",
        `Assoc [ ("name", `String "waq"); ("version", `String "0.1.0") ] );
      ("protocols", `List [ `String "activitypub" ]);
      ("services", `Assoc [ ("outbound", `List []); ("inbound", `List []) ]);
      ( "usage",
        `Assoc
          [
            ( "users",
              `Assoc
                [
                  ("total", `Int 1);
                  ("activeMonth", `Int 1);
                  ("activeHalfyear", `Int 1);
                ] );
            ("localPosts", `Int 1);
          ] );
      ("openRegistrations", `Bool false);
      ("metadata", `Assoc []);
    ]
  |> respond_yojson
