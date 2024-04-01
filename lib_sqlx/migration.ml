open Util

type operation = Connection.t -> unit Lwt.t

module type S = sig
  val change : (operation * operation) list -> (operation * operation) list
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
           Logs.info (fun m -> m "Migrate %d" id);
           c#transaction (fun c ->
               M.change [] |> List.rev_map fst |> Lwt_list.iter_s (fun f -> f c);%lwt
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
     Logs.info (fun m -> m "Rollback %d" last_v);
     let (module M : S) = migrations |> List.assoc last_v in
     let sql =
       "DELETE FROM " ^ config.schema_migrations ^ " WHERE version = $1"
     in
     c#transaction (fun c ->
         M.change [] |> List.map snd |> Lwt_list.iter_s (fun f -> f c);%lwt
         c#execute sql ~p:[ `Int last_v ])
     >|= fun r -> if not r then failwith "Rollback failed"

module Helper = struct
  module Internal = struct
    let create_table ~table_name ~schema (c : Connection.t) =
      Printf.sprintf {|CREATE TABLE %s ( %s )|} table_name
        (schema |> String.concat ", ")
      |> c#execute

    let drop_table ~table_name (c : Connection.t) =
      {|DROP TABLE |} ^ table_name |> c#execute

    let add_column ~table_name ~name ~spec (c : Connection.t) =
      Printf.sprintf {|ALTER TABLE %s ADD COLUMN %s %s|} table_name name spec
      |> c#execute

    let drop_column ~table_name ~name (c : Connection.t) =
      Printf.sprintf {|ALTER TABLE %s DROP COLUMN %s|} table_name name
      |> c#execute

    let create_index ~name ~table_name ~schema (c : Connection.t) =
      Printf.sprintf {|CREATE INDEX %s ON %s ( %s )|} name table_name
        (schema |> String.concat ", ")
      |> c#execute

    let create_unique_index ~name ~table_name ~schema (c : Connection.t) =
      Printf.sprintf {|CREATE UNIQUE INDEX %s ON %s ( %s )|} name table_name
        (schema |> String.concat ", ")
      |> c#execute

    let drop_index ~name (c : Connection.t) =
      Printf.sprintf {|DROP INDEX %s|} name |> c#execute
  end

  let create_table_not_model ~table_name ~schema acc =
    Internal.(create_table ~table_name ~schema, drop_table ~table_name) :: acc

  let create_table ~table_name ~schema acc =
    let schema =
      "id SERIAL PRIMARY KEY"
      :: "created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL"
      :: "updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL" :: schema
    in
    create_table_not_model ~table_name ~schema acc

  let add_column ~table_name ~name ~spec acc =
    Internal.(add_column ~table_name ~name ~spec, drop_column ~table_name ~name)
    :: acc

  let create_index ~name ~table_name ~schema acc =
    Internal.(create_index ~name ~table_name ~schema, drop_index ~name) :: acc

  let create_unique_index ~name ~table_name ~schema acc =
    Internal.(create_unique_index ~name ~table_name ~schema, drop_index ~name)
    :: acc

  let ( *> ) f g x = g (f x)
end
