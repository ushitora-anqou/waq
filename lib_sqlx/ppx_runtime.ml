open Util

class type connection =
  object ('a)
    method query : string -> Value.t list -> (string * Value.t) list list Lwt.t
    method query_row : string -> Value.t list -> (string * Value.t) list Lwt.t
    method execute : string -> Value.t list -> unit Lwt.t
    method enqueue_task_after_commit : ('a -> unit Lwt.t) -> unit Lwt.t
    method transaction : ('a -> unit Lwt.t) -> bool Lwt.t
    (*method enqueued_tasks_after_commit : ('a -> unit Lwt.t) list*)
  end

module Make (M : sig
  module ID : sig
    type t

    val to_int : t -> int
  end

  type column
  type t

  val columns : column list
  val string_of_column : column -> string
  val table_name : string
  val unpack : t -> (string * Value.t) list
  val pack : (string * Value.t) list -> t
  val id : t -> ID.t
  val after_create_commit_callbacks : (t -> connection -> unit Lwt.t) list ref
end) =
struct
  let where_id name ptn cond =
    match ptn with
    | None -> cond
    | Some ptn -> (
        match ptn with
        | `Eq (x : M.ID.t) ->
            Sql.where_int name (Some (`Eq (M.ID.to_int x))) cond
        | `In (vals : M.ID.t list) ->
            Sql.where_int name (Some (`In (vals |> List.map M.ID.to_int))) cond)

  let where_id_opt name ptn cond =
    match ptn with
    | None -> cond
    | Some (`Eq _ | `In _) as x -> where_id name x cond
    | Some ((`EqNone | `NeqNone) as x) -> Sql.where_nullable name x cond

  let preload chosen spec (c : connection) rows =
    match rows with
    | [] ->
        (* Prevent infinite loops. *)
        (* NOTE: It will NOT prevent all infinite loops,
           if there are any mutual recursive reference. *)
        Lwt.return []
    | _ ->
        spec
        |> Lwt_list.iter_s (fun (column, f) ->
               if List.mem column chosen then f rows c else Lwt.return_unit);%lwt
        Lwt.return rows

  let select id created_at updated_at order_by limit preload_chosen
      (c : connection) preload_spec cond =
    let sql, param =
      Sql.select ~columns:`Star ~table_name:M.table_name
        ~order_by:
          (order_by
          |> Option.map (List.map (fun (k, ad) -> (M.string_of_column k, ad))))
        ~limit
      @@ where_id "id" id
      @@ Sql.where_timestamp "created_at" created_at
      @@ Sql.where_timestamp "updated_at" updated_at
      @@ cond
    in
    c#query sql param >|= List.map M.pack
    >>= preload preload_chosen preload_spec c

  let count id created_at updated_at (c : connection) cond =
    let sql, param =
      Sql.select ~columns:`Count_star ~table_name:M.table_name ~order_by:None
        ~limit:None
      @@ where_id "id" id
      @@ Sql.where_timestamp "created_at" created_at
      @@ Sql.where_timestamp "updated_at" updated_at
      @@ cond
    in
    c#query_row sql param >|= List.hd >|= snd >|= Value.expect_int

  let update (xs : M.t list) (c : connection) preload_chosen preload_spec =
    xs
    |> Lwt_list.map_s (fun x ->
           let sql, param =
             Sql.update ~table_name:M.table_name
               ~columns:M.(columns |> List.map string_of_column)
               ~unpacked:(M.unpack x)
             @@ ([], [])
           in
           c#query_row sql param >|= M.pack)
    >>= preload preload_chosen preload_spec c

  let insert (xs : M.t list) (c : connection) preload_chosen preload_spec =
    (* FIXME: Efficient impl *)
    let%lwt rows =
      xs
      |> Lwt_list.map_s (fun x ->
             let sql, param =
               Sql.insert ~table_name:M.table_name
                 ~columns:M.(List.map string_of_column columns)
                 ~unpacked:(M.unpack x)
             in
             c#query_row sql param >|= M.pack)
    in
    rows
    |> Lwt_list.iter_s (fun row ->
           !M.after_create_commit_callbacks
           |> Lwt_list.iter_s (fun f -> c#enqueue_task_after_commit (f row)));%lwt
    preload preload_chosen preload_spec c rows

  let delete (xs : M.t list) (c : connection) =
    (* FIXME: Efficient impl *)
    xs
    |> Lwt_list.iter_p @@ fun x ->
       let sql, param =
         Sql.delete ~table_name:M.table_name ~id:(x |> M.id |> M.ID.to_int)
       in
       c#execute sql param
end

let expect_loaded = function None -> failwith "not preloaded" | Some x -> x
