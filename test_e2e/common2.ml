include Common

type agent = {
  kind : [ `Waq | `Mstdn ];
  token : string;
  username : string;
  domain : string;
}
[@@deriving make]

let lookup src = lookup src.kind ~token:src.token
let follow src = follow src.kind ~token:src.token
let post src = post src.kind ~token:src.token
let search src = search src.kind ~token:src.token
let reblog src = reblog src.kind ~token:src.token
let unreblog src = unreblog src.kind ~token:src.token
let home_timeline src = home_timeline src.kind ~token:src.token
let delete_status src = delete_status src.kind ~token:src.token
let get_status src = get_status src.kind ~token:src.token

let upload_media src ~filename ~data ~content_type =
  let target = "/api/v2/media" in
  let headers =
    [
      (`Accept, "application/json");
      (`Authorization, "Bearer " ^ src.token);
      ( `Content_type,
        "multipart/form-data; \
         boundary=---------------------------91791948726096252761377705945" );
    ]
  in
  let body =
    [ {|-----------------------------91791948726096252761377705945--|}; {||} ]
  in
  let body =
    [
      {|-----------------------------91791948726096252761377705945|};
      {|Content-Disposition: form-data; name="file"; filename="|} ^ filename
      ^ {|"|};
      {|Content-Type: |} ^ content_type;
      {||};
      data;
    ]
    @ body
  in
  assert (List.length body <> 2);
  let body = String.concat "\r\n" body in
  fetch_exn ~headers ~meth:`POST ~body (url src.kind target)
  >|= Yojson.Safe.from_string >|= media_attachment_of_yojson

let lookup_agent src dst =
  let domain = if src.domain = dst.domain then None else Some dst.domain in
  lookup src ~username:dst.username ?domain ()

let follow_agent src dst =
  let%lwt id, _, _ = lookup_agent src dst in
  follow src id

let expect_no_status src id =
  try%lwt
    get_status src id |> ignore_lwt;%lwt
    assert false
  with Httpq.Client.FetchFailure (Some (`Not_found, _, _)) -> Lwt.return_unit
