(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml

type job =
  { id : int
  ; solver : Solver_type.t
  }

type running =
  { job : job
  ; pid : int
  ; fd : Unix.file_descr
  ; started : Mtime_clock.counter
  }

let solver_name solver =
  String.lowercase_ascii (Fmt.str "%a" Solver_type.pp solver)

let timeout_ns timeout = Int64.mul (Int64.of_int timeout) 1_000_000L

let child ~timeout { id; solver } query fd =
  let set_timer s =
    ignore (Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = s })
  in
  let (module M) = Solver_dispatcher.mappings_of_solver solver in
  let module S = Solver.Batch (M) in
  match
    let s = S.create () in
    Sys.set_signal Sys.sigalrm Signal_default;
    set_timer (float_of_int timeout /. 1000.);
    let counter = Mtime_clock.counter () in
    let status =
      Fun.protect ~finally:(fun () -> set_timer 0.) (fun () -> S.check s query)
    in
    (status, Mtime.Span.to_uint64_ns (Mtime_clock.count counter))
  with
  | status, time ->
    let status =
      match status with
      | `Sat -> "sat"
      | `Unsat -> "unsat"
      | `Unknown -> "unknown"
    in
    let msg = Fmt.str "%s %Ld" status time in
    ignore (Unix.write_substring fd msg 0 (String.length msg));
    Unix._exit 0
  | exception e ->
    Fmt.epr "replay: query %d on %a: %s@." id Solver_type.pp solver
      (Printexc.to_string e);
    Unix._exit 1

let spawn ~timeout query job =
  let rd, wr = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 ->
    Unix.close rd;
    child ~timeout job query wr
  | pid ->
    Unix.close wr;
    { job; pid; fd = rd; started = Mtime_clock.counter () }

let result ~timeout r (status : Unix.process_status) =
  match status with
  | WSIGNALED s when s = Sys.sigalrm -> ("timeout", timeout_ns timeout)
  | _ ->
    let buf = Bytes.create 64 in
    let n = Unix.read r.fd buf 0 (Bytes.length buf) in
    let time =
      match String.split_on_char ' ' (Bytes.sub_string buf 0 n) with
      | [ status; time ] ->
        Option.map (fun time -> (status, time)) (Int64.of_string_opt time)
      | _ -> None
    in
    Option.value time
      ~default:("error", Mtime.Span.to_uint64_ns (Mtime_clock.count r.started))

let done_runs csv =
  let dones = Hashtbl.create 1024 in
  ( match Bos.OS.File.read_lines csv with
  | Error _ | Ok [] -> ()
  | Ok (_header :: rows) ->
    List.iter
      (fun row ->
        match String.split_on_char ',' row with
        | id :: _frequency :: solver :: _ ->
          Hashtbl.replace dones (id, solver) ()
        | _ -> () )
      rows );
  dones

let cmd ~solvers ~timeout ~jobs log output_csv =
  List.iter
    (fun solver ->
      let (module M) = Solver_dispatcher.mappings_of_solver solver in
      if not M.is_available then
        Fmt.failwith "Replay: %a is not available" Solver_type.pp solver )
    solvers;
  let queries =
    match Query_log.read log with
    | Ok queries -> Array.of_list (Query_log.unique queries)
    | Error (`Msg m) -> Fmt.failwith "%s" m
  in
  let feats =
    Array.map
      (fun (query, _) ->
        lazy (Feature.feats_to_strings (Feature_extraction.extract_feats query)) )
      queries
  in
  let dones = done_runs output_csv in
  let pending =
    List.concat
    @@ List.init (Array.length queries) (fun id ->
      List.filter_map
        (fun solver ->
          if Hashtbl.mem dones (string_of_int id, solver_name solver) then None
          else Some { id; solver } )
        solvers )
  in
  let nb_runs = List.length pending in
  Log.info (fun k ->
    k "Replaying %d runs (%d already done)" nb_runs (Hashtbl.length dones) );
  let oc =
    Out_channel.open_gen
      [ Open_wronly; Open_creat; Open_append; Open_text ]
      0o644
      (Fpath.to_string output_csv)
  in
  Fun.protect ~finally:(fun () -> Out_channel.close oc) @@ fun () ->
  if Int64.equal (Out_channel.length oc) 0L then
    Feature_extraction.output_row oc
      ( "query_id" :: "frequency" :: "solver" :: "status" :: "time"
      :: Feature.all_feature_names );
  let nb_done = ref 0 in
  let write { id; solver } (status, time) =
    incr nb_done;
    Feature_extraction.output_row oc
      ( string_of_int id
      :: string_of_int (snd queries.(id))
      :: solver_name solver :: status :: Int64.to_string time
      :: Lazy.force feats.(id) );
    Out_channel.flush oc;
    Log.info (fun k -> k "Replayed %d/%d" !nb_done nb_runs)
  in
  let rec loop pending running =
    match pending with
    | job :: pending when List.compare_length_with running jobs < 0 ->
      loop pending (spawn ~timeout (fst queries.(job.id)) job :: running)
    | _ when running = [] -> ()
    | _ ->
      let rec wait () =
        try Unix.wait () with Unix.Unix_error (EINTR, _, _) -> wait ()
      in
      let pid, status = wait () in
      let finished, running = List.partition (fun r -> r.pid = pid) running in
      List.iter
        (fun r ->
          let result = result ~timeout r status in
          Unix.close r.fd;
          write r.job result )
        finished;
      loop pending running
  in
  loop pending [];
  Log.debug (fun k -> k "Done writing to %a" Fpath.pp output_csv)
