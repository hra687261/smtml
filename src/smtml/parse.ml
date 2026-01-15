(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

exception Syntax_error of string

module Smtml = struct
  open Lexer
  open Lexing

  let pp_pos fmt lexbuf =
    let pos = lexbuf.lex_curr_p in
    Fmt.pf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)

  let parse_with_error lexbuf =
    try Parser.script Lexer.token lexbuf with
    | SyntaxError msg ->
      raise (Syntax_error (Fmt.str "%a: %s" pp_pos lexbuf msg))
    | Parser.Error ->
      raise (Syntax_error (Fmt.str "%a: syntax error" pp_pos lexbuf))

  let from_file filename =
    let res =
      Bos.OS.File.with_ic filename
        (fun chan () ->
          let lexbuf = Lexing.from_channel chan in
          lexbuf.lex_curr_p <-
            { lexbuf.lex_curr_p with pos_fname = Fpath.to_string filename };
          parse_with_error lexbuf )
        ()
    in
    match res with Error (`Msg e) -> Fmt.failwith "%s" e | Ok v -> v

  let from_string contents = parse_with_error (Lexing.from_string contents)
end

module Smtlib = struct
  open Smtlib

  let from_file filename =
    let parsed, loc = ([], Dolmen.Std.Loc.mk_file (Fpath.to_string filename)) in
    let base, filename = Fpath.split_base filename in
    let logic_file =
      State.mk_file ~loc (Fpath.to_string base)
        (`File (Fpath.to_string filename))
    in
    let response_file = State.mk_file "" (`File "this is unused") in
    let state =
      State.empty
      |> State.init ~debug:false ~report_style:Regular ~max_warn:max_int
           ~reports:(Dolmen_loop.Report.Conf.mk ~default:Enabled)
           ~response_file
             (* these limits are ignored in this example; to actually enforce
              the limits, one has to use the `run` function from
              `Dolmen_loop.Pipeline` *)
           ~time_limit:0. ~size_limit:0.
      |> State.set State.logic_file logic_file
      |> Typer.init
      |> Typer_loop.init ~type_check:true
    in

    let _final_state, rev_typed_stmts =
      List.fold_left
        (fun (state, acc) parsed_stmt ->
          let state, typed_stmts = Typer_loop.check state parsed_stmt in
          (state, List.rev_append typed_stmts acc) )
        (state, []) parsed
    in
    let typed_stmts = List.rev rev_typed_stmts in
    List.iter
      (fun typed_stmt -> Fmt.epr "%a@." Typer_loop.print typed_stmt)
      typed_stmts;
    (* TODO: insert translation *)
    assert false
end

let from_file filename =
  match Fpath.split_ext filename with
  | _, ".smtml" -> Smtml.from_file filename
  | _, ".smt2" -> Smtlib.from_file filename
  | fname, ext -> (
    (* FIXME: I don't like this *)
    match Fpath.to_string fname with
    | "-" -> Smtml.from_file filename
    | _ -> Fmt.failwith "Unsupported script type with extension '%s'" ext )
