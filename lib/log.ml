open Util

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

let reporters : reporter list ref = ref []
let add_reporter (r : reporter) : unit = reporters := r :: !reporters

let log (l : level) : 'a log =
 fun f ->
  !reporters
  |> List.iter @@ fun r ->
     if level_implies r.l l then
       f @@ fun fmt ->
       Format.fprintf r.fmt
         ("[%s][%s] @[" ^^ fmt ^^ "@]@.")
         Ptime.(now () |> to_rfc3339 ~space:true ~tz_offset_s:0)
         (string_of_level l)

let debug : 'a log = fun f -> log Debug f
let info : 'a log = fun f -> log Info f
let warn : 'a log = fun f -> log Warning f
let err : 'a log = fun f -> log Error f
