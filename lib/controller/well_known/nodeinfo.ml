open Helper

let get _ _req =
  `Assoc
    [
      ( "links",
        `List
          [
            `Assoc
              [
                ( "rel",
                  `String "http://nodeinfo.diaspora.software/ns/schema/2.0" );
                ( "href",
                  `String ("https://" ^ Config.server_name () ^ "/nodeinfo/2.0")
                );
              ];
          ] );
    ]
  |> respond_yojson
