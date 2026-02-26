(* We want these types for settings so that we can derive serializers for yaml/toml as well as cmdliner in the future *)

let backtrace =
  let doc =
    "Set to true the recording of backtraces (By default set to false)"
  in
  Cmdliner.Arg.(value & flag & info [ "b" ] ~doc)

let rec_backtrace backtrace = Printexc.record_backtrace backtrace

module Run = struct
  type t =
    { debug : bool
    ; dry : bool
    ; print_statistics : bool
    ; no_strict_status : bool
    ; solver_type : Smtml.Solver_type.t
    ; solver_mode : Smtml.Solver_mode.t
    ; from_file : Fpath.t option
    ; filenames : Fpath.t list [@main]
    }
  [@@deriving make]
end

module To_smt2 = struct
  type t =
    { debug : bool
    ; solver_type : Smtml.Solver_type.t
    ; filename : Fpath.t [@main]
    }
  [@@deriving make]
end
