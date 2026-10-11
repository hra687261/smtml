(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Feature_map

(* Uses hashes to memoize feats and heights and not redo it for expression for
   which it was already done *)
let rec extract_feats_aux memo (feats : t) (e : Expr.t) : int * t =
  match Hashtbl.find_opt memo e.Hc.tag with
  | Some height -> (height, feats)
  | None ->
    let feats = incr_feat (Feature.of_expr_kind e.node (Expr.ty e)) feats in
    let add_op_feats ty op feats =
      List.fold_left
        (fun feats f -> incr_feat f feats)
        feats (Feature.of_op ty op)
    in
    let feats, children =
      match Expr.view e with
      | Val _ | Symbol _ -> (feats, [])
      | Ptr { offset; _ } -> (feats, [ offset ])
      | List lst | App (_, lst) -> (feats, lst)
      | Naryop (ty, naryop, lst) ->
        let ty =
          match (naryop, lst) with Distinct, e :: _ -> Expr.ty e | _ -> ty
        in
        (add_op_feats ty (Feature.of_naryop naryop) feats, lst)
      | Unop (ty, unop, t) ->
        (add_op_feats ty (Feature.of_unop unop) feats, [ t ])
      | Cvtop (ty, cvtop, t) ->
        (add_op_feats ty (Feature.of_cvtop cvtop) feats, [ t ])
      | Extract (t, _, _) -> (feats, [ t ])
      | Binop (ty, binop, e1, e2) ->
        (add_op_feats ty (Feature.of_binop binop) feats, [ e1; e2 ])
      | Relop (ty, relop, e1, e2) ->
        (add_op_feats ty (Feature.of_relop relop) feats, [ e1; e2 ])
      | Concat (e1, e2) -> (feats, [ e1; e2 ])
      | Triop (ty, triop, e1, e2, e3) ->
        let ty = match triop with Ite -> Expr.ty e | _ -> ty in
        (add_op_feats ty (Feature.of_triop triop) feats, [ e1; e2; e3 ])
      | Binder (_, lst, t) -> (feats, t :: lst)
    in
    let height, feats =
      List.fold_left
        (fun (height, feats) child ->
          let child_height, feats = extract_feats_aux memo feats child in
          (Int.max height child_height, feats) )
        (0, feats) children
    in
    let height = height + 1 in
    Hashtbl.add memo e.Hc.tag height;
    (height, feats)

let extract_feats assertions : t =
  (* Memoization table for the heights and the features of visited expressions
   *)
  let memo = Hashtbl.create 64 in
  let feats, max_depth, depth_acc =
    List.fold_left
      (fun (feats, max_depth, depth_acc) expr ->
        let depth, feats = extract_feats_aux memo feats expr in
        (feats, Int.max max_depth depth, depth_acc + depth) )
      (empty, 0, 0) assertions
  in
  let nb_exprs = List.length assertions in
  add_nb_queries nb_exprs
  (* Rounding to nearest *)
  @@ add_mean_depth ((depth_acc + (nb_exprs / 2)) / nb_exprs)
  @@ rename_depth_to_max_depth (add_depth max_depth feats)

let output_row oc values =
  Out_channel.output_string oc (String.concat "," values);
  Out_channel.output_char oc '\n'
