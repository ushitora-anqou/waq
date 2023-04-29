open Util
open Lwt.Infix

let uri_re = Regex.e {|https?://[\x21\x24-\x3b\x3d\x3f-\x5f\x61-\x7a\x7c\x7e]*|}
(* FIXME: Use regular expressions defined in https://github.com/twitter/twitter-text/blob/30e2430d90cff3b46393ea54caf511441983c260/rb/lib/twitter-text/regex.rb *)

let kick (status : Model.Status.t) =
  Job.kick ~name:__FUNCTION__ @@ fun () ->
  (* Find (or insert if necessary) PreviewCard *)
  let%lwt cards_already_inserted, cards_not_inserted =
    Regex.match_ uri_re status#text
    |> List.map (fun a -> (Option.get a.(0)).Regex.substr)
    |> List.sort_uniq compare
    |> Lwt_list.filter_map_p (fun url ->
           match%lwt Db.(e PreviewCard.(get_one ~url) |> maybe_no_row) with
           | Some x -> Lwt.return_some x
           | None -> (
               let%lwt oembed_opt =
                 match%lwt Ogp.fetch_oembed_opt url with
                 | Some y -> Lwt.return_some y
                 | None -> Ogp.fetch_opengraph_opt url
               in
               match oembed_opt with
               | None -> Lwt.return_none
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
                     ~provider_url:x.provider_url ~width:x.width
                     ~height:x.height ~embed_url:x.embed_url ()
                   |> Lwt.return_some))
    >|= List.partition (fun c -> Option.is_some c#id_opt)
  in
  let%lwt cards_not_inserted = Db.(e PreviewCard.(insert cards_not_inserted)) in
  let cards = cards_already_inserted @ cards_not_inserted in

  (* Insert PreviewCardStatus (associative entity) *)
  let rels =
    cards
    |> List.map (fun card ->
           Model.PreviewCardStatus.make ~preview_card_id:card#id
             ~status_id:status#id ())
  in
  let%lwt _ = Db.(e PreviewCardStatus.(insert rels)) in

  Lwt.return_unit
