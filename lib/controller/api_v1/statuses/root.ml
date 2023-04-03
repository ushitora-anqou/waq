open Entity
open Util
open Lwt.Infix [@@warning "-33"]
open Helper

(* GET /api/v1/statuses/:id *)
let get req =
  let status_id =
    req |> Httpq.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  let%lwt self_id =
    may_authenticate_account req >|= fun a -> a |> Option.map (fun x -> x#id)
  in
  match%lwt Db.(e @@ Status.get_one ~id:status_id |> maybe_no_row) with
  | None -> Httpq.Server.raise_error_response `Not_found
  | Some s ->
      let%lwt s = make_status_from_model ?self_id s in
      s |> yojson_of_status |> Yojson.Safe.to_string
      |> Httpq.Server.respond ~headers:[ content_type_app_json ]

(* Recv POST /api/v1/statuses *)
let match_mention =
  let r =
    {|(?<=^|[^\w/\\])@([\w.-]+)(?:@([\w.-]+(?::[0-9]+)?))?(?:$|[^\w@\\.-])|}
    |> Regex.e
  in
  fun s ->
    Regex.match_ r s
    |> List.map (fun r ->
           let open Regex in
           let username = Option.get r.(1) in
           let domain = r.(2) in
           match domain with
           | None -> (username.offset, username.length, username.substr, None)
           | Some domain ->
               ( username.offset,
                 domain.offset + domain.length - username.offset,
                 username.substr,
                 Some domain.substr ))

let replace_mention spec text =
  let cur, subs =
    spec |> List.sort compare
    |> List.fold_left
         (fun (cur, subs) (off, len, subtext) ->
           (off + len, subtext :: String.sub text cur (off - cur) :: subs))
         (0, [])
  in
  String.(sub text cur (length text - cur)) :: subs
  |> List.rev |> String.concat ""

let post req =
  let%lwt self = authenticate_account req in
  let status =
    req
    |> Httpq.Server.query_opt "status"
    |> Option.value ~default:"" |> String.trim
  in
  let in_reply_to_id =
    req
    |> Httpq.Server.query_opt "in_reply_to_id"
    |> Option.map (fun s -> s |> int_of_string |> Model.Status.ID.of_int)
  in
  let media_ids = req |> Httpq.Server.query_many "media_ids" in

  (* Sanity check *)
  (match (status = "", media_ids) with
  | false, _ | true, _ :: _ -> ()
  | _ -> raise_error_response `Unprocessable_entity);

  (* Handle media attachments *)
  let%lwt attachments =
    let ids =
      media_ids |> List.map (int_of_string *> Model.MediaAttachment.ID.of_int)
    in
    Db.(e MediaAttachment.(select ~id:(`In ids)))
  in
  if List.length attachments <> List.length media_ids then
    raise_error_response `Bad_request;

  (* Handle mentions *)
  let%lwt mentioned_spots, mentioned_accts =
    match_mention status
    |> Lwt_list.filter_map_p (fun (off, len, username, domain) ->
           try%lwt
             Activity.search_account (`Webfinger (domain, username))
             >|= fun a -> Some ((off, len, a), a)
           with _ ->
             Logq.debug (fun m ->
                 m "Couldn't find the mentioned account: %s"
                   (username
                   ^ match domain with None -> "" | Some s -> "@" ^ s));
             Lwt.return_none)
    >|= List.split
  in
  let status =
    let spec =
      mentioned_spots
      |> List.map @@ fun (off, len, a) ->
         let open Jingoo.Jg_types in
         let models = [ ("username", Tstr a#username); ("uri", Tstr a#uri) ] in
         let text =
           Jingoo.Jg_template.from_string ~models
             {|<span class="h-card"><a href="{{ uri }}" class="u-url mention">@<span>{{ username }}</span></a></span>|}
         in
         (off, len, text)
    in
    replace_mention spec status
  in

  (* Insert status and mentions *)
  let%lwt s =
    Db.(
      e
        Status.(
          save_one_with_uri
            (make ~text:status ~uri:"" ~account_id:self#id ?in_reply_to_id ())))
  in
  (mentioned_accts
  |> Lwt_list.iter_p @@ fun acct ->
     Db.(e Mention.(make ~account_id:acct#id ~status_id:s#id () |> save_one))
     |> ignore_lwt);%lwt

  (* Update attachments *)
  ( attachments |> List.map (fun m -> m#with_status_id (Some s#id)) |> fun xs ->
    Db.(e MediaAttachment.(update xs)) |> ignore_lwt );%lwt

  (* Deliver the status to others *)
  Worker.Distribute.kick s;%lwt

  (* Return the result to the client *)
  let%lwt s = make_status_from_model ~self_id:self#id s in
  s |> yojson_of_status |> Yojson.Safe.to_string
  |> Httpq.Server.respond ~headers:[ content_type_app_json ]

let delete req =
  let%lwt self = authenticate_account req in
  let status_id =
    req |> Httpq.Server.param ":id" |> int_of_string |> Model.Status.ID.of_int
  in
  let%lwt status =
    try Db.e (Model.Status.get_one ~id:status_id)
    with Sqlx.Error.NoRowFound -> Httpq.Server.raise_error_response `Not_found
  in

  (* We should construct the result BEFORE the removal *)
  let%lwt status_to_be_returned =
    make_status_from_model ~self_id:self#id status
  in
  Worker.Removal.kick ~account_id:self#id ~status_id;%lwt
  yojson_of_status status_to_be_returned |> respond_yojson
