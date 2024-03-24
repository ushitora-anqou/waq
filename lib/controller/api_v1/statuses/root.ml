open Entity
open Util
open Helper

(* GET /api/v1/statuses/:id *)
let get _ req =
  let status_id =
    req |> Yume.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  let self_id = may_authenticate_account req |> Option.map (fun x -> x#id) in
  let s =
    try Db.(e @@ Status.get_one ~id:status_id)
    with Sqlx.Error.NoRowFound -> Yume.Server.raise_error_response `Not_found
  in
  let s = make_status_from_model ?self_id s in
  s |> yojson_of_status |> Yojson.Safe.to_string
  |> Yume.Server.respond ~headers:[ content_type_app_json ]

(* Recv POST /api/v1/statuses *)
let post env req =
  let self = authenticate_account req in
  let status = req |> Yume.Server.query ~default:"" "status" |> String.trim in
  let spoiler_text =
    req |> Yume.Server.query ~default:"" "spoiler_text" |> String.trim
  in
  let in_reply_to_id =
    req
    |> Yume.Server.query_opt "in_reply_to_id"
    |> Option.map (fun s -> s |> int_of_string |> Model.Status.ID.of_int)
  in
  let media_ids = req |> Yume.Server.query_many "media_ids" in

  (* Sanity check *)
  (match (status = "", media_ids) with
  | false, _ | true, _ :: _ -> ()
  | _ -> raise_error_response `Unprocessable_entity);

  (* Handle media attachments *)
  let attachments =
    let ids =
      media_ids |> List.map (int_of_string *> Model.MediaAttachment.ID.of_int)
    in
    Db.(e MediaAttachment.(select ~id:(`In ids)))
  in
  if List.length attachments <> List.length media_ids then
    raise_error_response `Bad_request;

  (* Handle mentions *)
  let mentioned_accts =
    Text_helper.match_mention status
    |> List.filter_map (fun (_off, _len, username, domain) ->
           try
             Some (Activity.search_account env (`Webfinger (domain, username)))
           with _ ->
             Logq.debug (fun m ->
                 m "Couldn't find the mentioned account: %s"
                   (username
                   ^ match domain with None -> "" | Some s -> "@" ^ s));
             None)
  in

  (* Insert status and mentions *)
  let s =
    Db.(
      e
        Status.(
          save_one_with_uri_and_url
            (make ~text:status ~uri:"" ~account_id:self#id ?in_reply_to_id
               ~spoiler_text ())))
  in
  (mentioned_accts
  |> List.iter @@ fun acct ->
     Db.(e Mention.(make ~account_id:acct#id ~status_id:s#id () |> save_one))
     |> ignore);

  (* Update attachments *)
  (let xs = attachments |> List.map (fun m -> m#with_status_id (Some s#id)) in
   Db.(e MediaAttachment.(update xs)) |> ignore);

  (* Deliver the status to others *)
  Worker.Distribute.kick env s;

  (* Attach preview cards if any *)
  Worker.Link_crawl.kick env s#id;

  (* Return the result to the client *)
  let s = make_status_from_model ~self_id:self#id s in
  s |> yojson_of_status |> Yojson.Safe.to_string
  |> Yume.Server.respond ~headers:[ content_type_app_json ]

let delete env req =
  let self = authenticate_account req in
  let status_id =
    req |> Yume.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  let status =
    try Db.e (Model.Status.get_one ~id:status_id)
    with Sqlx.Error.NoRowFound -> Yume.Server.raise_error_response `Not_found
  in
  if status#account_id <> self#id then raise_error_response `Not_found;

  (* We should construct the result BEFORE the removal *)
  let status_to_be_returned = make_status_from_model ~self_id:self#id status in
  Worker.Removal.kick env ~account_id:self#id ~status_id;
  yojson_of_status status_to_be_returned |> respond_yojson
