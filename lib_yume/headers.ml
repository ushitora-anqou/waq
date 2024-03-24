type t = Header.t list

let to_list : t -> (string * string) list = List.map Header.to_tuple
let of_list : (string * string) list -> t = List.map Header.of_tuple
