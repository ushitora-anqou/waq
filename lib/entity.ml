open Util

(* Entity account *)
type account = {
  id : string;
  username : string;
  acct : string;
  display_name : string;
  created_at : string;
}
[@@deriving make, yojson]

let make_account_from_model (a : Db.Account.t) =
  make_account ~id:(string_of_int a.id) ~username:a.username
    ~acct:(acct a.username a.domain) ~display_name:a.display_name
    ~created_at:(Ptime.to_rfc3339 a.created_at)

(* Entity CredentialAccount *)
type credential_account_source = { privacy : string; sensitive : bool }
[@@deriving make, yojson]

type credential_account = {
  id : string;
  username : string;
  acct : string;
  display_name : string;
  created_at : string;
  source : credential_account_source;
}
[@@deriving make, yojson]

let make_credential_account_from_model (a : Db.Account.t) : credential_account =
  let source =
    make_credential_account_source ~privacy:"public" ~sensitive:false
  in
  make_credential_account ~id:(string_of_int a.id) ~username:a.username
    ~acct:(acct a.username a.domain) ~display_name:a.display_name
    ~created_at:(Ptime.to_rfc3339 a.created_at)
    ~source

(* Entity status *)
type status = {
  id : string;
  created_at : string;
  visibility : string;
  uri : string;
  content : string;
  replies_count : int;
  in_reply_to_id : string option;
  in_reply_to_account_id : string option;
  account : account;
}
[@@deriving make, yojson]

let make_status_from_model ?(visibility = "public") (s : Db.Status.t) =
  let open Lwt.Infix in
  let%lwt in_reply_to_account_id =
    match s.in_reply_to_id with
    | None -> Lwt.return_none
    | Some _ -> (
        match%lwt Db.Status.get_one ~in_reply_to_id:s.in_reply_to_id () with
        | exception Sql.NoRowFound ->
            Httpq.Server.raise_error_response `Bad_request
        | s -> s.account_id |> string_of_int |> Lwt.return_some)
  in
  let%lwt replies_count = Db.Status.get_replies_count s.id in
  Db.Account.get_one ~id:s.account_id () >|= fun a ->
  let account = make_account_from_model a in
  make_status ~id:(string_of_int s.id)
    ~created_at:(Ptime.to_rfc3339 s.created_at)
    ~visibility ~uri:s.uri ~content:s.text ~account ~replies_count
    ?in_reply_to_id:(s.in_reply_to_id |> Option.map string_of_int)
    ?in_reply_to_account_id ()
