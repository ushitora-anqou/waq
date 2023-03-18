open Util

module type S = sig
  val up : Connection.t -> unit Lwt.t
  val down : Connection.t -> unit Lwt.t
end

type config = {
  schema_migrations : string;
      (* Table name to store the current status of the migrations *)
}

let get_commited_versions ~config (c : Connection.t) =
  let sql1 =
    "CREATE TABLE IF NOT EXISTS " ^ config.schema_migrations
    ^ " ( version BIGINT PRIMARY KEY )"
  in
  let sql2 =
    "SELECT version FROM " ^ config.schema_migrations ^ " ORDER BY version"
  in
  c#execute sql1;%lwt
  c#query sql2
  >|= List.map (function
        | [ ("version", v) ] -> Value.expect_int v
        | _ -> assert false)

let verify_migration_status ~config ~migrations c =
  get_commited_versions ~config c >|= fun versions ->
  let versions' = migrations |> List.map fst in
  if versions <> versions' then
    failwith "Verification of migration status failed"
(*
  (* versions' should be a prefix of versions *)
  let rec aux = function
    | [], _ -> true
    | x :: xs, y :: ys when x = y -> aux (xs, ys)
    | _ -> false
  in
  aux (versions', versions)
*)

let migrate ~migrations ~config (c : Connection.t) =
  (* FIXME: Not sure it's safe when it's called twice in parallel *)
  let%lwt versions = get_commited_versions ~config c in
  let rec aux = function
    | x :: commited, (y, _) :: cands when x = y -> aux (commited, cands)
    | [], cands ->
        let sql = "INSERT INTO " ^ config.schema_migrations ^ " VALUES ($1)" in
        cands
        |> Lwt_list.iter_s @@ fun (id, (module M : S)) ->
           Logq.info (fun m -> m "Migrate %d" id);
           c#transaction (fun c ->
               M.up c;%lwt
               c#execute sql ~p:[ `Int id ])
           >|= fun r -> if not r then failwith "Migration failed"
    | _ -> failwith "Migration error: current status is invalid"
  in
  aux (versions, migrations)

let rollback ~n ~migrations ~config (c : Connection.t) =
  let take n l =
    let rec aux acc = function
      | 0, _ -> List.rev acc
      | n, x :: xs -> aux (x :: acc) (n - 1, xs)
      | _ -> failwith "take: not enough elements"
    in
    aux [] (n, l)
  in
  let%lwt versions = get_commited_versions ~config c in
  versions |> List.rev |> take n
  |> Lwt_list.iter_s @@ fun last_v ->
     Logq.info (fun m -> m "Rollback %d" last_v);
     let (module M : S) = migrations |> List.assoc last_v in
     let sql =
       "DELETE FROM " ^ config.schema_migrations ^ " WHERE version = $1"
     in
     c#transaction (fun c ->
         M.down c;%lwt
         c#execute sql ~p:[ `Int last_v ])
     >|= fun r -> if not r then failwith "Rollback failed"
