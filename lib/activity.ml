open Lwt.Infix
open Util
module Uri = Httpq.Uri

let is_my_domain (u : string) =
  u |> Uri.of_string |> Uri.domain |> Config.is_my_domain

(* .well-known/webfinger *)
type webfinger_link = {
  rel : string;
  typ : string; [@key "type"]
  href : string;
}
[@@deriving make, yojson] [@@yojson.allow_extra_fields]

type webfinger = {
  subject : string;
  aliases : string list;
  links : json_any list;
}
[@@deriving make, yojson] [@@yojson.allow_extra_fields]

[@@@warning "-30"]

type ap_accept = { id : string; actor : Yojson.Safe.t; obj : t }

and ap_create = {
  id : string;
  actor : Yojson.Safe.t;
  published : string;
  to_ : string list;
  cc : string list;
  obj : t;
}

and ap_follow = { id : string; actor : string; obj : string }

and ap_document = {
  media_type : string option;
  url : string;
  name : string option;
  blurhash : string option;
  width : int option;
  height : int option;
}

and ap_note = {
  id : string;
  published : string;
  attributed_to : string;
  to_ : string list;
  cc : string list;
  content : string;
  in_reply_to : Yojson.Safe.t;
  attachment : t list;
  tag : Yojson.Safe.t list;
  summary : string;
  url : string option;
}

and ap_image = { url : string }

and ap_person = {
  id : string;
  following : string;
  followers : string;
  inbox : string;
  outbox : string;
  shared_inbox : string;
  preferred_username : string;
  name : string;
  summary : string;
  url : string;
  tag : string list;
  public_key_id : string;
  public_key_owner : string;
  public_key_pem : string;
  icon : ap_image option;
  image : ap_image option;
  is_service : bool;
}

and ap_undo = {
  id : string;
  actor : Yojson.Safe.t;
  to_ : string list option;
  obj : t;
}

and ap_announce = {
  id : string;
  actor : string;
  published : string;
  to_ : string list;
  cc : string list;
  obj : string;
}

and ap_like = { id : string; actor : string; obj : string }

and ap_delete = {
  id : string;
  actor : string;
  to_ : string list;
  obj : json_any;
}

and ap_tombstone = { id : string }

and ap_ordered_collection = {
  id : string;
  totalItems : int;
  first : string;
  last : string option;
}

and ap_update = { id : string; actor : string; to_ : string list; obj : t }

and t =
  | Accept of ap_accept
  | Announce of ap_announce
  | Document of ap_document
  | Create of ap_create
  | Delete of ap_delete
  | Follow of ap_follow
  | Like of ap_like
  | Note of ap_note
  | OrderedCollection of ap_ordered_collection
  | Person of ap_person
  | Tombstone of ap_tombstone
  | Undo of ap_undo
  | Update of ap_update

[@@@warning "+30"]

(* FIXME: The following functions should be generated by PPX *)
let get_accept = function Accept r -> Some r | _ -> None
let get_announce = function Announce r -> Some r | _ -> None
let get_create = function Create r -> Some r | _ -> None
let get_delete = function Delete r -> Some r | _ -> None
let get_document = function Document r -> Some r | _ -> None
let get_follow = function Follow r -> Some r | _ -> None
let get_like = function Like r -> Some r | _ -> None
let get_note = function Note r -> Some r | _ -> None
let get_person = function Person r -> Some r | _ -> None
let get_tombstone = function Tombstone r -> Some r | _ -> None
let get_undo = function Undo r -> Some r | _ -> None
let get_update = function Update r -> Some r | _ -> None
let of_accept r = Accept r
let of_note r = Note r
let accept r = Accept r
let announce r = Announce r
let create r = Create r
let delete r = Delete r
let document r = Document r
let follow r = Follow r
let like r = Like r
let note r = Note r
let ordered_collection r = OrderedCollection r
let person r = Person r
let tombstone r = Tombstone r
let undo r = Undo r
let update r = Update r
let make_follow ~id ~actor ~obj : ap_follow = { id; actor; obj }
let make_accept ~id ~actor ~obj : ap_accept = { id; actor; obj }
let make_undo ~id ~actor ~obj ?to_ () : ap_undo = { id; actor; obj; to_ }
let make_like ~id ~actor ~obj : ap_like = { id; actor; obj }
let make_delete ~id ~actor ~obj ~to_ : ap_delete = { id; actor; obj; to_ }
let make_tombstone ~id : ap_tombstone = { id }
let make_update ~id ~actor ~to_ ~obj : ap_update = { id; actor; to_; obj }
let make_image ~url : ap_image = { url }

let make_document ?media_type ~url ?name ?blurhash ?width ?height () :
    ap_document =
  { media_type; url; name; blurhash; width; height }

let make_announce ~id ~actor ~published ~to_ ~cc ~obj : ap_announce =
  { id; actor; published; to_; cc; obj }

let make_create ~id ~actor ~published ~to_ ~cc ~obj : ap_create =
  { id; actor; published; to_; cc; obj }

let make_note ~id ~published ~attributed_to ~to_ ~cc ~content ~in_reply_to
    ~attachment ~tag ~summary ?url () : ap_note =
  {
    id;
    published;
    attributed_to;
    to_;
    cc;
    content;
    in_reply_to;
    attachment;
    tag;
    summary;
    url;
  }

let make_ordered_collection ~id ~totalItems ~first ?last () :
    ap_ordered_collection =
  { id; totalItems; first; last }

let make_person ~id ~following ~followers ~inbox ~shared_inbox ~outbox
    ~preferred_username ~name ~summary ~url ~tag ~public_key_id
    ~public_key_owner ~public_key_pem ~icon ~image ~is_service =
  {
    id;
    following;
    followers;
    inbox;
    shared_inbox;
    outbox;
    preferred_username;
    name;
    summary;
    url;
    tag;
    public_key_id;
    public_key_owner;
    public_key_pem;
    icon;
    image;
    is_service;
  }

type property =
  | Id
  | Object
  | Actor
  | Type
  | Published
  | To
  | Cc
  | AttributedTo
  | Content
  | InReplyTo
  | Following
  | Followers
  | Inbox
  | SharedInbox
  | Outbox
  | PreferredUsername
  | Name
  | Summary
  | Url
  | Tag
  | PublicKey
  | Endpoints
  | TotalItems
  | First
  | Last
  | Icon
  | Image
  | MediaType
  | Blurhash
  | Width
  | Height
  | Attachment

let string_of_property : property -> string = function
  | Id -> "id"
  | Object -> "object"
  | Actor -> "actor"
  | Type -> "type"
  | Published -> "published"
  | To -> "to"
  | Cc -> "cc"
  | AttributedTo -> "attributedTo"
  | Content -> "content"
  | InReplyTo -> "inReplyTo"
  | Following -> "following"
  | Followers -> "followers"
  | Inbox -> "inbox"
  | SharedInbox -> "sharedInbox"
  | Outbox -> "outbox"
  | PreferredUsername -> "preferredUsername"
  | Name -> "name"
  | Summary -> "summary"
  | Url -> "url"
  | Tag -> "tag"
  | PublicKey -> "publicKey"
  | Endpoints -> "endpoints"
  | TotalItems -> "totalItems"
  | First -> "first"
  | Last -> "last"
  | Icon -> "icon"
  | Image -> "image"
  | MediaType -> "mediaType"
  | Blurhash -> "blurhash"
  | Width -> "width"
  | Height -> "height"
  | Attachment -> "attachment"

let rec of_yojson (src : Yojson.Safe.t) =
  let expect_assoc = function `Assoc l -> l | _ -> failwith "Expect assoc" in
  let expect_string = function
    | `String s -> s
    | _ -> failwith "Expect string"
  in
  let expect_list = function `List l -> l | _ -> failwith "Expect list" in
  let expect_int = function `Int i -> i | _ -> failwith "Expect int" in
  let l = expect_assoc src in
  let get name = l |> List.assoc (string_of_property name) in
  let assoc name = get name |> expect_assoc in
  let string name = get name |> expect_string in
  let list name = get name |> expect_list in
  let int name = get name |> expect_int in
  let string_opt name = try Some (get name |> expect_string) with _ -> None in
  let assoc_opt name = try Some (get name |> expect_assoc) with _ -> None in
  let image_opt name =
    assoc_opt name
    |> Option.map @@ fun x ->
       make_image ~url:(x |> List.assoc "url" |> expect_string)
  in
  let list_opt name = try Some (list name) with _ -> None in
  let get_opt name = try Some (get name) with _ -> None in
  let int_opt name = try Some (int name) with _ -> None in

  let typ = string Type in
  match typ with
  | "Accept" ->
      let id = string Id in
      make_accept ~id ~actor:(get Actor) ~obj:(get Object |> of_yojson)
      |> of_accept
  | "Announce" ->
      let id = string Id in
      let published = string Published in
      let to_ = list To |> List.map expect_string in
      let cc = list Cc |> List.map expect_string in
      let obj = string Object in
      make_announce ~id ~actor:(string Actor) ~published ~to_ ~cc ~obj
      |> announce
  | "Delete" ->
      let id = string Id in
      let actor = string Actor in
      let to_ = list To |> List.map expect_string in
      let obj = get Object in
      make_delete ~id ~actor ~to_ ~obj |> delete
  | "Tombstone" ->
      let id = string Id in
      make_tombstone ~id |> tombstone
  | "Follow" ->
      let id = string Id in
      make_follow ~id ~actor:(string Actor) ~obj:(string Object) |> follow
  | "Undo" ->
      let id = string Id in
      make_undo ~id ~actor:(get Actor)
        ~obj:(get Object |> of_yojson)
        ?to_:(try Some (list To |> List.map expect_string) with _ -> None)
        ()
      |> undo
  | "Create" ->
      let id = string Id in
      let published = string Published in
      let to_ = list To |> List.map expect_string in
      let cc = list Cc |> List.map expect_string in
      let obj = get Object |> of_yojson in
      make_create ~id ~actor:(get Actor) ~published ~to_ ~cc ~obj |> create
  | "Like" ->
      let id = string Id in
      make_like ~id ~actor:(string Actor) ~obj:(string Object) |> like
  | "Document" ->
      let media_type = string_opt MediaType in
      let url = string Url in
      let name = string_opt Name in
      let blurhash = string_opt Blurhash in
      let width = int_opt Width in
      let height = int_opt Height in
      make_document ?media_type ~url ?name ?blurhash ?width ?height ()
      |> document
  | "Note" ->
      let id = string Id in
      let published = string Published in
      let to_ = list To |> List.map expect_string in
      let cc = list Cc |> List.map expect_string in
      let attributed_to = string AttributedTo in
      let content = string Content in
      let in_reply_to = get_opt InReplyTo |> Option.value ~default:`Null in
      let attachment =
        list_opt Attachment |> Option.value ~default:[] |> List.map of_yojson
      in
      let tag = list Tag in
      let summary = string_opt Summary |> Option.value ~default:"" in
      let url = string_opt Url in
      make_note ~id ~published ~attributed_to ~to_ ~cc ~content ~in_reply_to
        ~attachment ~tag ~summary ?url ()
      |> note
  | "OrderedCollection" ->
      let id = string Id in
      let totalItems = int TotalItems in
      let first = string First in
      let last = string_opt Last in
      make_ordered_collection ~id ~totalItems ~first ?last ()
      |> ordered_collection
  | "Person" | "Service" ->
      let id = string Id in
      let following = string Following in
      let followers = string Followers in
      let inbox = string Inbox in
      let shared_inbox =
        match string SharedInbox with
        | s -> s
        | exception _ -> (
            match assoc Endpoints with
            | l ->
                l
                |> List.assoc (string_of_property SharedInbox)
                |> expect_string
            | exception _ -> "")
      in
      let outbox = string Outbox in
      let preferred_username = string PreferredUsername in
      let name = string Name in
      let summary = string_opt Summary |> Option.value ~default:"" in
      let url = string Url in
      let tag =
        (* FIXME
           list_opt Tag |> Option.value ~default:[] |> List.map expect_string
        *)
        []
      in
      let public_key = assoc PublicKey in
      let public_key_id = public_key |> List.assoc "id" |> expect_string in
      let public_key_owner =
        public_key |> List.assoc "owner" |> expect_string
      in
      let public_key_pem =
        public_key |> List.assoc "publicKeyPem" |> expect_string
      in
      let icon = image_opt Icon in
      let image = image_opt Image in
      let is_service = typ = "Service" in
      make_person ~id ~following ~followers ~inbox ~outbox ~shared_inbox
        ~preferred_username ~name ~summary ~url ~tag ~public_key_id
        ~public_key_owner ~public_key_pem ~icon ~image ~is_service
      |> person
  | "Update" ->
      let id = string Id in
      let actor = string Actor in
      let to_ = list To |> List.map expect_string in
      let obj = get Object |> of_yojson in
      make_update ~id ~actor ~to_ ~obj |> update
  | _ -> assert false

let rec to_yojson ?(context = Some "https://www.w3.org/ns/activitystreams") v =
  let to_yojson = to_yojson ~context:None in
  let string s = `String s in
  let l =
    match v with
    | Accept r ->
        [
          (Id, `String r.id);
          (Type, `String "Accept");
          (Actor, r.actor);
          (Object, to_yojson r.obj);
        ]
    | Announce r ->
        [
          (Id, `String r.id);
          (Type, `String "Announce");
          (Actor, `String r.actor);
          (Published, `String r.published);
          (To, `List (r.to_ |> List.map string));
          (Cc, `List (r.cc |> List.map string));
          (Object, `String r.obj);
        ]
    | Create r ->
        [
          (Id, `String r.id);
          (Type, `String "Create");
          (Actor, r.actor);
          (Published, `String r.published);
          (To, `List (r.to_ |> List.map string));
          (Cc, `List (r.cc |> List.map string));
          (Object, to_yojson r.obj);
        ]
    | Delete r ->
        [
          (Id, `String r.id);
          (Type, `String "Delete");
          (Actor, `String r.actor);
          (To, `List (r.to_ |> List.map string));
          (Object, r.obj);
        ]
    | Follow r ->
        [
          (Id, `String r.id);
          (Type, `String "Follow");
          (Actor, `String r.actor);
          (Object, `String r.obj);
        ]
    | Like r ->
        [
          (Id, `String r.id);
          (Type, `String "Like");
          (Actor, `String r.actor);
          (Object, `String r.obj);
        ]
    | Note r ->
        [
          (Id, `String r.id);
          (Type, `String "Note");
          (Published, `String r.published);
          (AttributedTo, `String r.attributed_to);
          (To, `List (r.to_ |> List.map string));
          (Cc, `List (r.cc |> List.map string));
          (Content, `String r.content);
          (InReplyTo, r.in_reply_to);
          (Attachment, `List (r.attachment |> List.map to_yojson));
          (Tag, `List r.tag);
          (Summary, `String r.summary);
        ]
    | OrderedCollection r ->
        let l =
          [
            (Id, `String r.id);
            (Type, `String "OrderedCollection");
            (TotalItems, `Int r.totalItems);
            (First, `String r.first);
          ]
        in
        let l =
          match r.last with None -> l | Some last -> (Last, `String last) :: l
        in
        l
    | Person r ->
        let l =
          [
            (Id, `String r.id);
            (Type, `String (if r.is_service then "Service" else "Person"));
            (Following, `String r.following);
            (Followers, `String r.followers);
            (Inbox, `String r.inbox);
            (SharedInbox, `String r.shared_inbox);
            (Outbox, `String r.outbox);
            (PreferredUsername, `String r.preferred_username);
            (Name, `String r.name);
            (Summary, `String r.summary);
            (Url, `String r.url);
            (Tag, `List (r.tag |> List.map string));
            ( PublicKey,
              `Assoc
                [
                  ("id", `String r.public_key_id);
                  ("owner", `String r.public_key_owner);
                  ("publicKeyPem", `String r.public_key_pem);
                ] );
          ]
        in
        let image (i : ap_image) =
          `Assoc [ ("type", `String "Image"); ("url", `String i.url) ]
        in
        let l =
          r.icon |> Option.fold ~none:l ~some:(fun i -> (Icon, image i) :: l)
        in
        let l =
          r.image |> Option.fold ~none:l ~some:(fun i -> (Image, image i) :: l)
        in
        l
    | Tombstone r -> [ (Id, `String r.id); (Type, `String "Tombstone") ]
    | Undo r ->
        let l =
          [
            (Id, `String r.id);
            (Type, `String "Undo");
            (Actor, r.actor);
            (Object, to_yojson r.obj);
          ]
        in
        let l =
          r.to_
          |> Option.fold ~none:l ~some:(fun xs ->
                 (To, `List (xs |> List.map (fun x -> `String x))) :: l)
        in
        l
    | Update r ->
        [
          (Id, `String r.id);
          (Type, `String "Update");
          (Actor, `String r.actor);
          (To, `List (r.to_ |> List.map string));
          (Object, to_yojson r.obj);
        ]
    | Document r ->
        let may_cons ctor ?(null = false) k v_opt xs =
          v_opt
          |> Option.fold
               ~none:(if null then (k, `Null) :: xs else xs)
               ~some:(fun v -> (k, ctor v) :: xs)
        in
        let may_cons_string = may_cons (fun v -> `String v) in
        let may_cons_int = may_cons (fun v -> `Int v) in
        [ (Type, `String "Document"); (Url, `String r.url) ]
        |> may_cons_string MediaType r.media_type
        |> may_cons_string ~null:true Name r.name
        |> may_cons_string Blurhash r.blurhash
        |> may_cons_int Width r.width
        |> may_cons_int Height r.height
  in
  let l = l |> List.map (fun (k, v) -> (string_of_property k, v)) in
  let l =
    context
    |> Option.fold ~none:l ~some:(fun context ->
           ("@context", `String context) :: l)
  in
  assert (l |> List.mem_assoc "type");
  `Assoc l

let sign_spec_of_account (src : Db.Account.t) =
  let priv_key =
    src#private_key |> Option.get |> Httpq.Signature.decode_private_key
  in
  let key_id = src#uri ^ "#main-key" in
  let signed_headers =
    [ "(request-target)"; "host"; "date"; "digest"; "content-type" ]
  in
  Some (priv_key, key_id, signed_headers)

let post_activity_json ~body ~sign ~url =
  let meth = `POST in
  let headers = [ (`Content_type, "application/activity+json") ] in
  let%lwt res = Throttle_fetch.f ~meth ~headers ~body ~sign url in
  Lwt.return
  &
  match res with
  | Ok (status, _, _body)
    when Cohttp.Code.(status |> code_of_status |> is_success) ->
      ()
  | _ -> failwith "Failed to post activity json"

(* Get activity+json from the Internet *)
let fetch_activity ~uri =
  match uri with
  | "https://www.w3.org/ns/activitystreams" -> failwith "Not valid activity URI"
  | _ ->
      Throttle_fetch.f_exn
        ~headers:
          [
            (`Accept, "application/activity+json");
            (`Content_type, "text/html" (* dummy *));
          ]
        uri
      >|= Yojson.Safe.from_string

(* Send GET /.well-known/webfinger *)
let get_webfinger ~scheme ~domain ~username =
  (* FIXME: Check /.well-known/host-meta if necessary *)
  let%lwt body =
    Throttle_fetch.f_exn @@ scheme ^ ":/" ^/ domain
    ^/ ".well-known/webfinger?resource=acct:" ^ username ^ "@" ^ domain
  in
  body |> Yojson.Safe.from_string |> webfinger_of_yojson |> Lwt.return

let model_account_of_person ?original (r : ap_person) : Model.Account.t =
  let domain = Uri.(r.id |> of_string |> domain) |> Option.some in
  let avatar_remote_url = r.icon |> Option.map (fun (x : ap_image) -> x.url) in
  let header_remote_url =
    r.image |> Option.fold ~none:"" ~some:(fun (x : ap_image) -> x.url)
  in
  match original with
  | None ->
      Model.Account.make ~username:r.preferred_username ?domain
        ~public_key:r.public_key_pem ~display_name:r.name ~uri:r.id ~url:r.url
        ~inbox_url:r.inbox ~outbox_url:r.outbox ~followers_url:r.followers
        ~shared_inbox_url:r.shared_inbox ?avatar_remote_url ~header_remote_url
        ~note:r.summary ()
  | Some (a : Model.Account.t) ->
      let a = Oo.copy a in
      a#set_username r.preferred_username;
      a#set_domain domain;
      a#set_public_key r.public_key_pem;
      a#set_display_name r.name;
      a#set_note r.summary;
      a#set_uri r.id;
      a#set_url (Some r.url);
      a#set_inbox_url r.inbox;
      a#set_outbox_url r.outbox;
      a#set_followers_url r.followers;
      a#set_shared_inbox_url r.shared_inbox;
      a#set_avatar_remote_url avatar_remote_url;
      a#set_header_remote_url header_remote_url;
      a#set_actor_type (Some (if r.is_service then `Service else `Person));
      a

let fetch_person by =
  let get_uri = function
    | `DomainUser (domain, username) | `UserDomain (username, domain) ->
        get_webfinger ~scheme:"https" ~domain ~username >|= fun webfinger ->
        webfinger.links
        |> List.find_map @@ fun l ->
           let l = webfinger_link_of_yojson l in
           if l.rel = "self" then Some l.href else None
    | `Uri uri ->
        Uri.(with_fragment (of_string uri) None |> to_string)
        |> Option.some |> Lwt.return
  in
  match%lwt get_uri by with
  | None -> failwith "Couldn't find person's uri"
  | Some uri -> (
      fetch_activity ~uri >|= of_yojson >|= get_person >|= function
      | None -> failwith "Couldn't parse the activity Person"
      | Some person -> person)

let search_account ?(resolve = true) by : Model.Account.t Lwt.t =
  let make_new_account by =
    let%lwt a = fetch_person by >|= model_account_of_person in
    Db.(e @@ Account.save_one a)
  in
  match by with
  | `Webfinger (domain, username) -> (
      let domain =
        match domain with
        | Some s when s <> Config.server_name () -> Some s
        | _ -> None
      in
      match%lwt Db.(e @@ Account.get_one ~domain ~username) with
      | acc -> Lwt.return acc
      | exception Sqlx.Error.NoRowFound
        when domain = None (* Local *) || not resolve ->
          failwith "Couldn't find the account"
      | exception Sqlx.Error.NoRowFound ->
          make_new_account (`DomainUser (Option.get domain, username)))
  | `Uri uri -> (
      let uri = Uri.(with_fragment (of_string uri) None |> to_string) in
      match%lwt Db.(e @@ Account.get_one ~uri) with
      | acct -> Lwt.return acct
      | exception Sqlx.Error.NoRowFound when not resolve ->
          failwith "Couldn't find the account"
      | exception Sqlx.Error.NoRowFound -> make_new_account (`Uri uri))

let search_account_opt ?resolve by =
  try%lwt search_account ?resolve by >>= Lwt.return_some
  with _ -> Lwt.return_none

let verify_activity_json req =
  let%lwt body = Httpq.Server.body req in
  let signature = Httpq.Server.header `Signature req in
  let headers = Httpq.Server.headers req in
  let path = Httpq.Server.path req in
  let meth = Httpq.Server.meth req in

  let open Httpq.Signature in
  let { key_id; algorithm; headers = signed_headers; signature } =
    parse_signature_header signature
  in
  search_account_opt (`Uri key_id) >|= function
  | None -> (body, Error `AccountNotFound)
  | Some acct when Model.Account.is_local acct -> (body, Error `AccountIsLocal)
  | Some acct -> (
      let pub_key = decode_public_key acct#public_key in
      match
        verify ~pub_key ~algorithm ~signed_headers ~signature ~headers ~meth
          ~path ~body:(Some body)
      with
      | Error e -> (body, Error (`VerifFailure e))
      | Ok () -> (body, Ok ()))

let serialize_status (s : Model.Status.t) (self : Model.Account.t) : ap_note =
  let in_reply_to_s = s#in_reply_to in
  let published = s#created_at |> Ptime.to_rfc3339 in
  let to_ = [ "https://www.w3.org/ns/activitystreams#Public" ] in
  let mentions = s#mentions in
  let cc =
    mentions
    |> List.filter_map (fun m -> m#account)
    |> List.map (fun a -> a#uri)
    |> List.cons self#followers_url
  in
  let in_reply_to =
    in_reply_to_s |> Option.fold ~none:`Null ~some:(fun s -> `String s#uri)
  in
  let media_attachments = Model.Status.sorted_attachments s in
  let attachment =
    media_attachments
    |> List.map @@ fun ma ->
       let id = Model.MediaAttachment.ID.to_int ma#id in
       let url =
         ma#file_file_name
         |> Option.fold ~none:ma#remote_url ~some:(fun n ->
                K.original_media_attachments_url (id, n))
       in
       make_document ~url () |> document
  in
  {
    id = s#uri;
    published;
    to_;
    cc;
    attributed_to = self#uri;
    content = Text_helper.format_status_text s;
    in_reply_to;
    attachment;
    tag =
      mentions
      |> List.map (fun m ->
             let a = Option.get m#account in
             `Assoc
               [
                 ("type", `String "Mention");
                 ("href", `String a#uri);
                 ("name", `String ("@" ^ Model.Account.acct a));
               ]);
    summary = s#spoiler_text;
    url = s#url;
  }

let note_of_status (s : Db.Status.t) : ap_note Lwt.t =
  let%lwt self = Db.(e Account.(get_one ~id:s#account_id)) in
  let%lwt s =
    Db.(
      e
        Status.(
          get_one ~id:s#id
            ~preload:
              [
                `attachments [];
                `in_reply_to [];
                `mentions [ `account [] ];
                `account [];
              ]))
  in
  serialize_status s self |> Lwt.return

let rec status_of_note' (note : ap_note) : Db.Status.t Lwt.t =
  let published, _, _ = Ptime.of_rfc3339 note.published |> Result.get_ok in
  let%lwt in_reply_to_id =
    let uri_to_status_id uri = fetch_status ~uri >|= fun s -> Some s#id in
    match note.in_reply_to with
    | `String uri -> uri_to_status_id uri
    | `Assoc l -> (
        match l |> List.assoc_opt "id" with
        | Some (`String uri) -> uri_to_status_id uri
        | _ -> Lwt.return_none)
    | _ -> Lwt.return_none
  in
  let%lwt attributedTo = search_account (`Uri note.attributed_to) in

  let%lwt status =
    Db.(
      e
        Status.(
          make ~uri:note.id ~text:note.content ~created_at:published
            ~updated_at:published ~account_id:attributedTo#id ?in_reply_to_id
            ~spoiler_text:note.summary ?url:note.url ()
          |> save_one))
  in

  (* Handle attachments *)
  note.attachment
  |> List.filter_map get_document
  |> Lwt_list.iter_s (fun (d : ap_document) ->
         let%lwt blurhash =
           match (d.blurhash, d.url) with
           | Some h, _ -> Lwt.return h
           | None, "" -> Lwt.return Image.dummy_blurhash
           | None, url ->
               Throttle_fetch.http_get url >>= Image.inspect
               >|= fun (_, _, h) -> h
         in
         Db.(
           e
             MediaAttachment.(
               make ~type_:0 ~remote_url:d.url ~account_id:status#account_id
                 ~status_id:status#id ~blurhash ()
               |> save_one))
         |> ignore_lwt);%lwt

  (* Handle mentions *)
  (note.cc @ note.to_
  |> Lwt_list.filter_map_p (fun uri -> search_account_opt (`Uri uri))
  >>= Lwt_list.iter_p @@ fun acct ->
      let m =
        Model.Mention.(make ~account_id:acct#id ~status_id:status#id ())
      in
      Db.(e @@ Mention.(save_one m)) |> ignore_lwt);%lwt

  Lwt.return status

and status_of_note (note : ap_note) : Db.Status.t Lwt.t =
  match%lwt Db.(e Status.(get_one ~uri:note.id)) with
  | s -> Lwt.return s
  | exception Sqlx.Error.NoRowFound -> status_of_note' note

and status_of_announce' (ann : ap_announce) : Db.Status.t Lwt.t =
  let published, _, _ = Ptime.of_rfc3339 ann.published |> Result.get_ok in
  let%lwt reblogee = fetch_status ~uri:ann.obj in
  let%lwt account = search_account (`Uri ann.actor) in
  Db.(
    e
      Status.(
        make ~uri:ann.id ~text:"" ~created_at:published ~updated_at:published
          ~account_id:account#id ~reblog_of_id:reblogee#id ~spoiler_text:"" ()
        |> save_one))

and status_of_announce (ann : ap_announce) : Db.Status.t Lwt.t =
  match%lwt Db.(e Status.(get_one ~uri:ann.id)) with
  | s -> Lwt.return s
  | exception Sqlx.Error.NoRowFound -> status_of_announce' ann

and fetch_status ~uri =
  match%lwt Db.(e Status.(get_one ~uri)) with
  | s -> Lwt.return s
  | exception Sqlx.Error.NoRowFound -> (
      match%lwt fetch_activity ~uri >|= of_yojson with
      | Note note -> status_of_note note
      | Announce ann -> status_of_announce ann
      | _ -> failwith "fetch_status failed: fetched activity is invalid")

let create_note_of_status (s : Db.Status.t) : ap_create Lwt.t =
  let%lwt self = Db.(e Account.(get_one ~id:s#account_id)) in
  note_of_status s >|= fun note ->
  {
    id = note.id ^/ "activity";
    actor = `String self#uri;
    published = note.published;
    to_ = note.to_;
    cc = note.cc;
    obj = Note note;
  }

let announce_of_status ?(deleted = false) (s : Db.Status.t) : ap_announce Lwt.t
    =
  let%lwt reblog =
    let id = Option.get s#reblog_of_id in
    Db.(if deleted then e Status.(get_one' ~id) else e Status.(get_one ~id))
  in
  let%lwt reblog_acct = Db.(e Account.(get_one ~id:reblog#account_id)) in
  let%lwt self = Db.(e Account.(get_one ~id:s#account_id)) in

  let id = s#uri ^/ "activity" in
  let actor = self#uri in
  let published = s#created_at |> Ptime.to_rfc3339 in
  let to_ = [ "https://www.w3.org/ns/activitystreams#Public" ] in
  let cc = [ reblog_acct#uri; self#followers_url ] in
  let obj = reblog#uri in

  make_announce ~id ~actor ~published ~to_ ~cc ~obj |> Lwt.return

let like_of_favourite (f : Db.Favourite.t) : ap_like Lwt.t =
  let%lwt acct = Db.(e Account.(get_one ~id:f#account_id)) in
  let%lwt status = Db.(e Status.(get_one ~id:f#status_id)) in

  let id =
    acct#uri ^ "#likes/" ^ (f#id |> Model.Favourite.ID.to_int |> string_of_int)
  in
  let actor = acct#uri in
  let obj = status#uri in

  make_like ~id ~actor ~obj |> Lwt.return

let favourite_of_like ?(must_already_exist = false) (l : ap_like) :
    Db.Favourite.t Lwt.t =
  let%lwt acct = Db.(e Account.(get_one ~uri:l.actor)) in
  let%lwt status = Db.(e Status.(get_one ~uri:l.obj)) in
  let now = Ptime.now () in
  match%lwt
    Db.(e Favourite.(get_one ~account_id:acct#id ~status_id:status#id))
  with
  | fav -> Lwt.return fav
  | exception Sqlx.Error.NoRowFound ->
      if must_already_exist then
        failwith "favourite_of_like: must_already_exist failed"
      else
        Db.(
          e
            Favourite.(
              make ~created_at:now ~updated_at:now ~account_id:acct#id
                ~status_id:status#id ()
              |> save_one))

let to_undo ~actor =
  let actor = `String actor in
  function
  | Like v as a ->
      let id = v.id ^/ "undo" in
      make_undo ~id ~actor ~obj:a ()
  | Follow _ ->
      (* Undo Follow is not straightforward and should be handled separately in each case *)
      assert false
  | Announce v as a ->
      let id = v.id ^/ "undo" in
      make_undo ~id ~actor
        ~to_:[ "https://www.w3.org/ns/activitystreams#Public" ]
        ~obj:a ()
  | _ -> assert false

let person_of_account (a : Db.Account.t) : ap_person =
  make_person ~id:a#uri ~following:(a#uri ^/ "following")
    ~followers:a#followers_url ~inbox:a#inbox_url
    ~shared_inbox:a#shared_inbox_url ~outbox:a#outbox_url
    ~preferred_username:a#username ~name:a#display_name ~summary:a#note
    ~url:a#uri ~tag:[] ~public_key_id:(a#uri ^ "#main-key")
    ~public_key_owner:a#uri ~public_key_pem:a#public_key
    ~icon:(a#avatar_remote_url |> Option.map (fun url -> make_image ~url))
    ~image:
      (match a#header_remote_url with
      | "" -> None
      | url -> Some (make_image ~url))
    ~is_service:(a#actor_type = Some `Service)
