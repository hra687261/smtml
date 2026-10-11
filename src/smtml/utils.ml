(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

let run_and_time_call ~use f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let stop = Unix.gettimeofday () in
  use (stop -. start);
  result

let query_log_path : Fpath.t option =
  let env_var = "QUERY_LOG_PATH" in
  match Bos.OS.Env.var env_var with Some p -> Some (Fpath.v p) | None -> None

let[@inline never] protect m f =
  Mutex.lock m;
  match f () with
  | x ->
    Mutex.unlock m;
    x
  | exception e ->
    Mutex.unlock m;
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace e bt

(* If the environment variable [QUERY_LOG_PATH] is set, stores and writes
   all queries sent to the solver to the given file *)
let log_query : Expr.t list -> unit =
  match query_log_path with
  | None -> fun _ -> ()
  | Some path ->
    let log_entries : Expr.t list list Atomic.t = Atomic.make [] in
    let rec update_entries f =
      let entries = Atomic.get log_entries in
      if not (Atomic.compare_and_set log_entries entries (f entries)) then
        update_entries f
    in
    let close () =
      (* Take the entries, so that they are not written again when sigterm is
         called after at_exit *)
      let entries = Atomic.exchange log_entries [] in
      if List.compare_length_with entries 0 <> 0 then (
        try
          let oc =
            (* open with wr/r/r rights, create if it does not exit and append to
            it if it exists. *)
            Out_channel.open_gen
              [ Open_creat; Open_binary; Open_append ]
              0o644 (Fpath.to_string path)
          in
          Marshal.to_channel oc entries [];
          Out_channel.close oc
        with e ->
          (* If the run raises, put back the entries *)
          update_entries (fun newer -> newer @ entries);
          Fmt.failwith "Failed to write log: %s@." (Printexc.to_string e) )
    in
    at_exit close;
    Sys.set_signal Sys.sigterm
      (Sys.Signal_handle
         (fun _ ->
           close ();
           exit 143 ) );
    fun query -> update_entries (fun entries -> query :: entries)
