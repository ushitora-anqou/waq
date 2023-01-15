let compile ptn = Re.(Pcre.re ptn |> compile)

let match_group r s =
  match Re.exec_opt r s with
  | None -> Error "Match failed"
  | Some g -> Ok (Re.Group.all g |> Array.to_list)
