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

type subst = { off : int; len : int; subtext : string } [@@deriving make]

let substitute (spec : subst list) text =
  let cur, subs =
    spec |> List.stable_sort compare
    |> List.fold_left
         (fun (cur, subs) { off; len; subtext } ->
           if off < cur then (cur, subs)
           else (off + len, subtext :: String.sub text cur (off - cur) :: subs))
         (0, [])
  in
  String.(sub text cur (length text - cur)) :: subs
  |> List.rev |> String.concat ""

let match_urls =
  (* FIXME: Use regular expressions defined in https://github.com/twitter/twitter-text/blob/30e2430d90cff3b46393ea54caf511441983c260/rb/lib/twitter-text/regex.rb *)
  let uri_re =
    Regex.e {|https?://[\x21\x24-\x3b\x3d\x3f-\x5f\x61-\x7a\x7c\x7e]*|}
  in
  Regex.match_ uri_re *> List.map (fun a -> Option.get a.(0))

let format_status_text (status : Model.Status.t) =
  (* `status` should be preloaded with [ `mentions [ `account [] ]; `account [] ] *)
  if Model.Account.is_remote status#account then status#text
  else
    (* Handle links *)
    let subst_links =
      status#text |> match_urls
      |> List.map (fun g ->
             let open Jingoo.Jg_types in
             let html =
               Jingoo.Jg_template.from_string
                 ~models:[ ("url", Tstr g.Regex.substr) ]
                 {|<a href="{{ url }}" target="_blank" rel="nofollow noopener noreferrer">{{ url }}</a>|}
             in
             make_subst ~off:g.offset ~len:g.length ~subtext:html)
    in

    (* Handle mentions *)
    let subst_mentions =
      let tbl =
        status#mentions
        |> List.filter_map (fun m -> m#account)
        |> index_by (fun a -> (a#username, a#domain))
      in
      match_mention status#text
      |> List.filter_map @@ fun (off, len, username, domain) ->
         let open Jingoo.Jg_types in
         Hashtbl.find_opt tbl (username, domain)
         |> Option.map @@ fun a ->
            let models =
              [
                ("username", Tstr username);
                ("url", Tstr (a#url |> Option.value ~default:a#uri));
              ]
            in
            let text =
              Jingoo.Jg_template.from_string ~models
                {|<span class="h-card"><a href="{{ url }}" class="u-url mention">@<span>{{ username }}</span></a></span>|}
            in
            make_subst ~off ~len ~subtext:text
    in

    substitute (subst_links @ subst_mentions) status#text

let eliminate_html_tags = Soup.(parse *> texts *> String.concat "")
