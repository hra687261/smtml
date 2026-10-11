(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

(* The memo is keyed by tag but compares the expressions physically: the
   hash-consing is not domain-safe, so in a program with several domains (e.g.
   owi) different expressions can have the same tag *)
let rec rehash memo (e : Expr.t) : Expr.t =
  match List.assq_opt e (Hashtbl.find_all memo e.Hc.tag) with
  | Some e -> e
  | None ->
    let rehash = rehash memo in
    let rehashed =
      match Expr.view e with
      | Val v -> Expr.value v
      | Ptr { base; offset } ->
        Expr.ptr (Bitvector.to_int32 base) (rehash offset)
      | Symbol s -> Expr.symbol s
      | List l -> Expr.list (List.map rehash l)
      | App (s, l) -> Expr.app s (List.map rehash l)
      | Unop (ty, op, e) -> Expr.raw_unop ty op (rehash e)
      | Binop (ty, op, e1, e2) -> Expr.raw_binop ty op (rehash e1) (rehash e2)
      | Triop (ty, op, e1, e2, e3) ->
        Expr.raw_triop ty op (rehash e1) (rehash e2) (rehash e3)
      | Relop (ty, op, e1, e2) -> Expr.raw_relop ty op (rehash e1) (rehash e2)
      | Cvtop (ty, op, e) -> Expr.raw_cvtop ty op (rehash e)
      | Naryop (ty, op, l) -> Expr.raw_naryop ty op (List.map rehash l)
      | Extract (e, high, low) -> Expr.raw_extract (rehash e) ~high ~low
      | Concat (e1, e2) -> Expr.raw_concat (rehash e1) (rehash e2)
      | Binder (b, vars, e) -> Expr.binder b (List.map rehash vars) (rehash e)
    in
    Hashtbl.add memo e.Hc.tag (e, rehashed);
    rehashed

let read path =
  Bos.OS.File.with_ic path
    (fun ic () ->
      let rec read_query acc =
        match (Marshal.from_channel ic : Expr.t list list) with
        | l ->
          let memo = Hashtbl.create 1024 in
          read_query (List.map (List.map (rehash memo)) l @ acc)
        | exception End_of_file -> List.rev acc
      in
      read_query [] )
    ()

let unique queries =
  let tbl = Hashtbl.create 1024 in
  List.iter
    (function
      | [] -> ()
      | query -> (
        let key =
          Expr.hash
            (Expr.raw_naryop Ty_bool Logand (List.sort_uniq Expr.compare query))
          (* Deduplication with sorting is sound, but in practice reodering
           asserts sometimes changes the behavior/performance of smt solvers
           so maybe we should experiment without this. *)
        in
        match Hashtbl.find_opt tbl key with
        | Some (id, query, n) -> Hashtbl.replace tbl key (id, query, n + 1)
        | None -> Hashtbl.add tbl key (Hashtbl.length tbl, query, 1) ) )
    queries;
  let uniques = Array.make (Hashtbl.length tbl) ([], 0) in
  Hashtbl.iter (fun _ (id, query, n) -> uniques.(id) <- (query, n)) tbl;
  Array.to_list uniques
