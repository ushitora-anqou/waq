open Util

let extract_urls (status : Model.Status.t) =
  let urls =
    if Model.Account.is_local status#account then
      Text_helper.match_urls status#text |> List.map (fun g -> g.Regex.substr)
    else
      let open Soup in
      let soup = parse status#text in
      soup $$ "a" |> to_list
      |> List.filter_map (fun anchor ->
             let href = R.attribute "href" anchor in
             let rel =
               anchor |> attribute "rel"
               |> Option.fold ~none:false
                    ~some:
                      (String.split_on_char ' '
                      *> List.find_opt (( = ) "tag")
                      *> Option.is_some)
             in
             let cls =
               anchor |> attribute "class"
               |> Option.fold ~none:false
                    ~some:
                      (String.split_on_char ' '
                      *> List.find_opt (fun x -> x = "u-url" || x = "h-card")
                      *> Option.is_some)
             in
             let mention =
               status#mentions
               |> List.exists (fun m ->
                      m#account
                      |> Option.fold ~none:false ~some:(fun a ->
                             href = (a#url |> Option.value ~default:a#uri)))
             in
             if rel || cls || mention then None else Some href)
  in
  urls |> List.sort_uniq compare

let kick env (status_id : Model.Status.ID.t) =
  Job.kick env ~name:__FUNCTION__ @@ fun () ->
  let status =
    Db.(
      e
        Status.(
          get_one
            ~preload:[ `account []; `mentions [ `account [] ] ]
            ~id:status_id))
  in

  (* Find (or insert if necessary) PreviewCard *)
  let cards_already_inserted, cards_not_inserted =
    extract_urls status
    |> List.filter_map (fun url ->
           try Some Db.(e PreviewCard.(get_one ~url))
           with Sqlx.Error.NoRowFound -> (
             let oembed_opt =
               match Ogp.fetch_oembed_opt env url with
               | Some y -> Some y
               | None -> Ogp.fetch_opengraph_opt env url
             in
             match oembed_opt with
             | None -> None
             | Some x ->
                 (* Convert x into PreviewCard *)
                 let type_ =
                   match x.typ with
                   | "link" -> 0
                   | "photo" -> 1
                   | "video" -> 2
                   | "rich" -> 3
                   | _ -> failwith "Invalid oembed type"
                 in
                 Model.PreviewCard.make ~url:x.url ~title:x.title
                   ~description:x.description ?image_url:x.image ~type_
                   ~html:x.html ~author_name:x.author_name
                   ~author_url:x.author_url ~provider_name:x.provider_name
                   ~provider_url:x.provider_url ~width:x.width ~height:x.height
                   ~embed_url:x.embed_url ?blurhash:x.blurhash ()
                 |> Option.some))
    |> List.partition (fun c -> Option.is_some c#id_opt)
  in
  let cards_not_inserted = Db.(e PreviewCard.(insert cards_not_inserted)) in
  let cards = cards_already_inserted @ cards_not_inserted in

  (* Insert PreviewCardStatus (associative entity) *)
  let rels =
    cards
    |> List.map (fun card ->
           Model.PreviewCardStatus.make ~preview_card_id:card#id
             ~status_id:status#id ())
  in
  if
    Lwt_eio.run_lwt @@ fun () ->
    Db.(
      transaction (fun c ->
          let open PreviewCardStatus in
          let%lwt cards = get_many ~status_id c in
          if cards <> [] then delete cards c else Lwt.return_unit;%lwt
          PreviewCardStatus.insert rels c |> ignore_lwt))
  then ()
  else failwith "Transaction failed"
