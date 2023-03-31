type group = { offset : int; length : int; substr : string } [@@deriving make]

let e ptn = Pcre.regexp ptn

let match_ rex s =
  (try Pcre.exec_all ~rex s with Not_found -> [||])
  |> Array.map (fun sub ->
         Pcre.get_substrings sub
         |> Array.mapi (fun i s ->
                (try Some (Pcre.get_substring_ofs sub i)
                 with Not_found -> None)
                |> Option.map (fun (off1, off2) ->
                       make_group ~offset:(off1 - 1)
                         ~length:(off2 - off1 + 1)
                         ~substr:s)))
  |> Array.to_list
