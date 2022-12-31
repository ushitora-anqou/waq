type level = Debug | Info | Warning | Error [@@deriving enum]

let initialize (l : level) =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level
    (Some
       (match l with
       | Debug -> Debug
       | Info -> Info
       | Warning -> Warning
       | Error -> Error));
  print_newline ();
  ()

let debug = Logs.debug
let info = Logs.info
let warn = Logs.warn
let err = Logs.err
