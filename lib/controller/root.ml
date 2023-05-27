open Helper

let get _req =
  let open Jingoo.Jg_types in
  let models = [ ("server_name", Tstr (Config.server_name ())) ] in
  {|
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>{{ server_name }} - Waq</title>
</meta>
</head>
<body>
<h1>{{ server_name }} - Waq</h1>
<p><a href="https://github.com/ushitora-anqou/waq">Waq</a> is a pretty new ActivityPub (AP) server implementation written in OCaml. Please let me (<a href="https://mstdn.anqou.net/@anqou">@anqou@mstdn.anqou.net<a>) know if you encounter any problems with Waq, especially when communicating with other AP server implementations.</p>
</body>
</html>
  |}
  |> Jingoo.Jg_template.from_string ~models
  |> String.trim |> respond_html
