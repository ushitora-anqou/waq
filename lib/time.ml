type timezone = [ `Local | `UTC ]
type t = { t : Unix.tm; z : timezone }
type week = [ `Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat ]

let string_of_week (w : week) =
  match w with
  | `Sun -> "Sun"
  | `Mon -> "Mon"
  | `Tue -> "Tue"
  | `Wed -> "Wed"
  | `Thu -> "Thu"
  | `Fri -> "Fri"
  | `Sat -> "Sat"

type month =
  [ `Jan
  | `Feb
  | `Mar
  | `Apr
  | `May
  | `Jun
  | `Jul
  | `Aug
  | `Sep
  | `Oct
  | `Nov
  | `Dec ]

let string_of_month (m : month) =
  match m with
  | `Jan -> "Jan"
  | `Feb -> "Feb"
  | `Mar -> "Mar"
  | `Apr -> "Apr"
  | `May -> "May"
  | `Jun -> "Jun"
  | `Jul -> "Jul"
  | `Aug -> "Aug"
  | `Sep -> "Sep"
  | `Oct -> "Oct"
  | `Nov -> "Nov"
  | `Dec -> "Dec"

let make ~(year : int) ~(month : month) ~(day : int) ~(hour : int)
    ~(minute : int) ~(second : int) ~(z : timezone) : t =
  let t =
    Unix.(
      {
        tm_sec = second;
        tm_min = minute;
        tm_hour = hour;
        tm_mday = day;
        tm_mon =
          (match month with
          | `Jan -> 0
          | `Feb -> 1
          | `Mar -> 2
          | `Apr -> 3
          | `May -> 4
          | `Jun -> 5
          | `Jul -> 6
          | `Aug -> 7
          | `Sep -> 8
          | `Oct -> 9
          | `Nov -> 10
          | `Dec -> 11);
        tm_year = year - 1900;
        tm_wday = 0;
        tm_yday = 0;
        tm_isdst = false;
      }
      |> mktime |> snd)
  in
  { t; z }

let now () : t = { t = Unix.(time () |> gmtime); z = `UTC }

let to_localtime (v : t) : t =
  match v.z with
  | `Local -> v
  | `UTC -> { t = Unix.(mktime v.t |> fst |> localtime); z = `Local }

let to_utc (v : t) : t =
  match v.z with
  | `UTC -> v
  | `Local -> { t = Unix.(mktime v.t |> fst |> gmtime); z = `UTC }

let year (v : t) = v.t.tm_year + 1900

let month (v : t) : month =
  match v.t.tm_mon with
  | 0 -> `Jan
  | 1 -> `Feb
  | 2 -> `Mar
  | 3 -> `Apr
  | 4 -> `May
  | 5 -> `Jun
  | 6 -> `Jul
  | 7 -> `Aug
  | 8 -> `Sep
  | 9 -> `Oct
  | 10 -> `Nov
  | 11 -> `Dec
  | _ -> assert false

let day_of_month (v : t) = v.t.tm_mday
let hour (v : t) = v.t.tm_hour
let minute (v : t) = v.t.tm_min
let second (v : t) = v.t.tm_sec

let day_of_week (v : t) : week =
  match v.t.tm_wday with
  | 0 -> `Sun
  | 1 -> `Mon
  | 2 -> `Tue
  | 3 -> `Wed
  | 4 -> `Thu
  | 5 -> `Fri
  | 6 -> `Sat
  | _ -> assert false

let to_http_date (v : t) : string =
  let v = to_utc v in
  let day_name = v |> day_of_week |> string_of_week in
  let month = v |> month |> string_of_month in
  Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d GMT" day_name (day_of_month v)
    month (year v) (hour v) (minute v) (second v)

let to_string (v : t) : string =
  Printf.sprintf "%d/%s/%02d %02d:%02d:%02d" (year v)
    (month v |> string_of_month)
    (day_of_month v) (hour v) (minute v) (second v)
