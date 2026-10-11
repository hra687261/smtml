(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

val extract_feats : Expr.t list -> Feature_map.t

val output_row : Out_channel.t -> string list -> unit
