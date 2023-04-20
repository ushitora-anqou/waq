type t = Cohttp.Code.status_code

let to_string = Cohttp.Code.string_of_status
let is_error (s : t) = Cohttp.Code.(code_of_status s |> is_error)
let is_success (s : t) = Cohttp.Code.(code_of_status s |> is_success)
