(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Cmdliner

let ocaml_file =
  let parse s =
    match Sys.file_exists s with
    | true ->
        if Sys.is_directory s (* || Filename.extension s <> ".mli" *) then
          `Error (Printf.sprintf "Error: `%s' is not an OCaml interface file" s)
        else `Ok s
    | false -> `Error (Printf.sprintf "Error: `%s' not found" s)
  in
  (parse, Format.pp_print_string)

let print_intermediate =
  let doc = "Print all intermediate forms." in
  Arg.(value & flag & info [ "print-intermediate" ] ~doc)

let print_parsed =
  let doc = "Print after parsing." in
  Arg.(value & flag & info [ "print-parsed" ] ~doc)

let parse_only =
  let doc = "Stop after parsing." in
  Arg.(value & flag & info [ "parse-only" ] ~doc)

let parse_ocaml_only =
  let doc = "Stop after parsing OCaml signatures." in
  Arg.(value & flag & info [ "parse-ocaml-only" ] ~doc)

let bench_mode =
  let doc = "Run with bench mode (no intermediate printing, result summary)." in
  Arg.(value & flag & info [ "bench-mode" ] ~doc)

let backtrace =
  let doc = "Enable backtrace recording and reporting in case or error." in
  Arg.(value & flag & info [ "backtrace" ] ~doc)

let load_path =
  let doc = "Include directory in load path." in
  Arg.(value & opt_all dir [] & info [ "L"; "load-path" ] ~doc ~docv:"DIR")

let files = Arg.(non_empty & pos_all ocaml_file [] & info [] ~docv:"FILE")

let run_tc backtrace bench_mode print_intermediate print_parsed parse_only
    parse_ocaml_only load_path file =
  let load_path =
    List.fold_left
      (fun acc f ->
        let dir = Filename.dirname f in
        if not (List.mem dir acc) then dir :: acc else acc)
      load_path file
  in
  Printexc.record_backtrace backtrace;
  Tc.run
    {
      bench_mode;
      print_intermediate;
      print_parsed;
      parse_only;
      parse_ocaml_only;
      load_path;
    }
    file

let tc =
  let doc = "Run Gospel type-checker." in
  ( Term.(
      const run_tc
      $ backtrace
      $ bench_mode
      $ print_intermediate
      $ print_parsed
      $ parse_only
      $ parse_ocaml_only
      $ load_path
      $ files),
    Term.info "tc" ~doc )

let pps =
  let doc = "Run Gospel preprocessor." in
  (Term.(const Pps.run $ files), Term.info "pps" ~doc)

let wc =
  let doc = "Run Gospel line count." in
  (Term.(const Wc.run $ files), Term.info "wc" ~doc)

let usage_cmd =
  let doc = "The Gospel command line tool." in
  (Term.(ret (const (`Help (`Auto, None)))), Term.info "gospel" ~doc)

let () =
  let commands = [ tc; wc; pps ] in
  Term.(exit @@ eval_choice usage_cmd commands)
