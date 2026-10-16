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

let child ?logic ~timeout { id; solver } query fd =
  let set_timer s =
    ignore (Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = s })
  in
  let (module M) = Solver_dispatcher.mappings_of_solver solver in
  let module S = Solver.Batch (M) in
  let write msg = ignore (Unix.write_substring fd msg 0 (String.length msg)) in
  match
    write
      ( String.concat ","
          (Feature.feats_to_strings (Feature_extraction.extract_feats query))
      ^ "\n" );
    let s = S.create ?logic () in
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
    write (Fmt.str "%s %Ld" status time);
    Unix._exit 0
  | exception e ->
    Fmt.epr "replay: query %d on %a: %s@." id Solver_type.pp solver
      (Printexc.to_string e);
    if Printexc.backtrace_status () then Printexc.print_backtrace stderr;
    Unix._exit 1

let spawn ?logic ~timeout query job =
  let rd, wr = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 ->
    Unix.close rd;
    child ?logic ~timeout job query wr
  | pid ->
    Unix.close wr;
    { job; pid; fd = rd; started = Mtime_clock.counter () }

let read_all fd =
  let buf = Buffer.create 4096 in
  let bytes = Bytes.create 4096 in
  let rec loop () =
    match Unix.read fd bytes 0 (Bytes.length bytes) with
    | 0 -> Buffer.contents buf
    | n ->
      Buffer.add_subbytes buf bytes 0 n;
      loop ()
    | exception Unix.Unix_error (EINTR, _, _) -> loop ()
  in
  loop ()

(* The child writes the features of the query on a first line, then the
   status and the time *)
let result ~timeout r (status : Unix.process_status) =
  let feats, outcome =
    match String.split_on_char '\n' (read_all r.fd) with
    | [ feats; outcome ] -> (feats, outcome)
    | _ -> ("", "")
  in
  match status with
  | WSIGNALED s when s = Sys.sigalrm -> ("timeout", timeout_ns timeout, feats)
  | _ -> (
    let time =
      match String.split_on_char ' ' outcome with
      | [ status; time ] ->
        Option.map (fun time -> (status, time)) (Int64.of_string_opt time)
      | _ -> None
    in
    match time with
    | Some (status, time) -> (status, time, feats)
    | None ->
      ( match status with
      | WEXITED 1 -> (* The child printed the exception *) ()
      | WEXITED n ->
        Fmt.epr "replay: query %d on %a: exited with %d@." r.job.id
          Solver_type.pp r.job.solver n
      | WSIGNALED n | WSTOPPED n ->
        Fmt.epr "replay: query %d on %a: killed by signal %d@." r.job.id
          Solver_type.pp r.job.solver n );
      ("error", Mtime.Span.to_uint64_ns (Mtime_clock.count r.started), feats) )

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

let read_queries ~solvers log =
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
  (* Free the duplicates: every run forks this process, and the cost of a fork
     grows with its memory *)
  Gc.compact ();
  queries

let rec wait_pid pid =
  try Unix.waitpid [] pid with Unix.Unix_error (EINTR, _, _) -> wait_pid pid

let rerun ?logic ~solvers ~timeout log ids =
  let queries = read_queries ~solvers log in
  Printexc.record_backtrace true;
  List.iter
    (fun id ->
      if id < 0 || id >= Array.length queries then
        Fmt.failwith "Rerun: there are only %d queries" (Array.length queries);
      let query, frequency = queries.(id) in
      Fmt.pr "query %d (frequency %d, %d assertions)@." id frequency
        (List.length query);
      List.iter
        (fun solver ->
          let r = spawn ?logic ~timeout query { id; solver } in
          let _, status = wait_pid r.pid in
          let status, time, _feats = result ~timeout r status in
          Unix.close r.fd;
          Fmt.pr "  %a: %s %Ld ns@." Solver_type.pp solver status time )
        solvers )
    ids

let cmd ?logic ~solvers ~timeout ~jobs log output_csv =
  let queries = read_queries ~solvers log in
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
  let oc =
    Out_channel.open_gen
      [ Open_wronly; Open_creat; Open_append; Open_text ]
      0o644
      (Fpath.to_string output_csv)
  in
  Log.app (fun k -> k "Started writing to %a" Fpath.pp output_csv);
  Fun.protect ~finally:(fun () -> Out_channel.close oc) @@ fun () ->
  if Int64.equal (Out_channel.length oc) 0L then
    Feature_extraction.output_row oc
      ( "query_id" :: "frequency" :: "solver" :: "status" :: "time"
      :: Feature.all_feature_names );
  let nb_done = ref 0 in
  let write { id; solver } (status, time, feats) =
    incr nb_done;
    Feature_extraction.output_row oc
      ( string_of_int id
      :: string_of_int (snd queries.(id))
      :: solver_name solver :: status :: Int64.to_string time :: [ feats ] );
    Out_channel.flush oc;
    Log.info (fun k -> k "Replayed %d/%d" !nb_done nb_runs)
  in
  let rec loop pending running =
    match pending with
    | job :: pending when List.compare_length_with running jobs < 0 ->
      loop pending (spawn ?logic ~timeout (fst queries.(job.id)) job :: running)
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
  Log.app (fun k ->
    k "Started replaying %d runs (%d already done)" nb_runs
      (Hashtbl.length dones) );
  loop pending [];
  Log.app (fun k -> k "Stopped replaying");
  Log.app (fun k -> k "Done writing to %a" Fpath.pp output_csv)
