open Util

let match_mention =
  let r =
    Regex.e
      {|(?<=^|[^\w/\\])@([\w.-]+)(?:@([\w.-]+(?::[0-9]+)?))?(?:$|[^\w@\\.-])|}
  in
  fun s ->
    Regex.match_ r s
    |> List.map (fun r ->
           let open Regex in
           let username = Option.get r.(1) in
           let domain = r.(2) in
           match domain with
           | None ->
               (username.offset - 1, username.length + 1, username.substr, None)
           | Some domain ->
               ( username.offset - 1,
                 domain.offset + domain.length - username.offset + 1,
                 username.substr,
                 if domain.substr = Config.server_name () then None
                 else Some domain.substr ))

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

let format_status_text (status : Model.Status.t) =
  (* FIXME: Handle links *)
  let tbl =
    status#mentions
    |> List.filter_map (fun m -> m#account)
    |> index_by (fun a -> (a#username, a#domain))
  in
  let spec =
    match_mention status#text
    |> List.map @@ fun (off, len, username, domain) ->
       let open Jingoo.Jg_types in
       let a = Hashtbl.find tbl (username, domain) in
       let models = [ ("username", Tstr username); ("uri", Tstr a#uri) ] in
       let text =
         Jingoo.Jg_template.from_string ~models
           {|<span class="h-card"><a href="{{ uri }}" class="u-url mention">@<span>{{ username }}</span></a></span>|}
       in
       (off, len, text)
  in
  replace_mention spec status#text
