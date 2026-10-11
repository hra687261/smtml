(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

val read : Fpath.t -> (Expr.t list list, [> `Msg of string ]) result

val unique : Expr.t list list -> (Expr.t list * int) list
