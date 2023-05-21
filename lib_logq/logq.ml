type level = Debug | Info | Warning | Error [@@deriving enum]

let level_implies l1 l2 = level_to_enum l1 <= level_to_enum l2

let string_of_level = function
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warning -> "WARN"
  | Error -> "ERROR"

type ('a, 'b) msgf = (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
type 'a log = ('a, unit) msgf -> unit

type reporter = {
  l : level;
  fmt : Format.formatter; [@default Format.err_formatter]
}
[@@deriving make]

let make_stderr_reporter ~l = make_reporter ~l ~fmt:Format.err_formatter ()

let make_file_reporter ~l ~file_name =
  let oc = open_out_gen [ Open_append; Open_creat ] 0o664 file_name in
  let fmt = Format.formatter_of_out_channel oc in
  make_reporter ~l ~fmt

let reporters : reporter list ref = ref []
let add_reporter (r : reporter) : unit = reporters := r :: !reporters

let log (l : level) : 'a log =
  let now () = Unix.gettimeofday () |> Ptime.of_float_s |> Option.get in
  fun f ->
    !reporters
    |> List.iter @@ fun r ->
       if level_implies r.l l then
         f @@ fun fmt ->
         Format.fprintf r.fmt
           ("[%s][%s] @[" ^^ fmt ^^ "@]@.")
           (now () |> Ptime.to_rfc3339 ~space:true ~tz_offset_s:0)
           (string_of_level l)

let debug : 'a log = fun f -> log Debug f
let info : 'a log = fun f -> log Info f
let warn : 'a log = fun f -> log Warning f
let err : 'a log = fun f -> log Error f
