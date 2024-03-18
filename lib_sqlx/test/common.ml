let with_postgres ~container_name ~host_port f =
  match
    Unix.system
      ("docker run -d --rm --name " ^ container_name
     ^ " -e POSTGRES_PASSWORD=password -p 127.0.0.1:" ^ string_of_int host_port
     ^ ":5432 postgres:16 &> /dev/null")
  with
  | Unix.WEXITED 0 ->
      Unix.sleep 20;
      Fun.protect
        ~finally:(fun () ->
          Unix.system ("docker stop " ^ container_name ^ " &> /dev/null")
          |> ignore)
        (fun () ->
          let dsn =
            "postgresql://postgres:password@localhost:"
            ^ string_of_int host_port ^ "/postgres"
          in
          f dsn)
  | _ -> assert false
