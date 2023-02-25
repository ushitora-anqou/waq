open Helper
open Lwt.Infix
open Util

let respond_activity a () =
  Activity.(person_of_account a |> person |> to_yojson) |> respond_yojson

let respond_html (a : Db.Account.t) () =
  let open Jingoo.Jg_types in
  let%lwt statuses =
    Db.account_statuses ~id:a.id ~limit:30 ~max_id:None ~since_id:None
      ~exclude_replies:false
    >|= List.map (fun (s : Db.Status.t) -> s.id)
    >>= Entity.serialize_statuses
  in
  let status_to_Tobj (s : Entity.status) =
    let rec aux (s : Entity.status) =
      match s.reblog with
      | Some s' ->
          aux s' |> List.remove_assoc "kind" |> List.cons ("kind", Tstr "reblog")
      | None ->
          [
            ("kind", Tstr "post");
            ("content", Tstr s.content);
            ("acct", Tstr s.account.acct);
            ( "rel_created_at",
              Tstr
                (let open Ptime in
                let created_at, _, _ =
                  s.created_at |> of_rfc3339 |> Result.get_ok
                in
                let secs =
                  diff (now ()) created_at |> Span.to_float_s |> int_of_float
                in
                if secs < 60 then string_of_int secs ^ " seconds ago"
                else if secs < 60 * 60 then
                  string_of_int (secs / 60) ^ " minutes ago"
                else if secs < 60 * 60 * 24 then
                  string_of_int (secs / 60 / 60) ^ " hours ago"
                else string_of_int (secs / 60 / 60 / 24) ^ " days ago") );
          ]
    in
    Tobj (aux s)
  in
  let models =
    [
      ("username", Tstr a.username);
      ( "domain",
        Tstr (a.domain |> Option.value ~default:(Config.server_name ())) );
      ("display_name", Tstr a.display_name);
      ("statuses", Tlist (statuses |> List.map status_to_Tobj));
    ]
  in
  {|
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>{{ display_name }} (@{{ username }}@{{ domain }}) - Waq</title>
</head>
<body>
<h1>{{ display_name }}</h1>
<h2>@{{ username }}@{{ domain }}</h2>
<p>Latest 30 posts:</p>
<ul>
{% for s in statuses %}
<li>{{ s.kind | capitalize }}: {{ s.content }} (@{{ s.acct }}, {{ s.rel_created_at }})</li>
{% endfor %}
</ul>
<p>Note: <a href="https://github.com/ushitora-anqou/waq">Waq</a> is a pretty new ActivityPub (AP) server implementation written in OCaml. Please let me (<a href="https://mstdn.anqou.net/@anqou">@anqou@mstdn.anqou.net<a>) know if you encounter any problems with Waq, especially when communicating with other AP server implementations.</p>
</body>
</html>
      |}
  |> Jingoo.Jg_template.from_string ~models
  |> String.trim
  |> Httpq.Server.respond ~status:`OK

(* Recv GET /users/:name *)
let get req =
  let username = req |> Httpq.Server.param ":name" in
  match%lwt Db.Account.get_one ~domain:None ~username () with
  | exception Sql.NoRowFound -> Httpq.Server.raise_error_response `Not_found
  | a ->
      let%lwt _ = Db.User.get_one ~account_id:a.id () in
      req
      |> render ~default:(respond_html a)
           [ (app_activity_json, respond_activity a) ]
