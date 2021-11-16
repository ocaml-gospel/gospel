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
        if Sys.is_directory s || Filename.extension s <> ".mli" then
          `Error (Printf.sprintf "Error: `%s' is not an OCaml interface file" s)
        else `Ok s
    | false -> `Error (Printf.sprintf "Error: `%s' not found" s)
  in
  (parse, Format.pp_print_string)

let verbose =
  let doc = "Print all intermediate forms." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let load_path =
  let doc = "Include directory in load path." in
  Arg.(value & opt_all dir [] & info [ "L"; "load-path" ] ~doc ~docv:"DIR")

let files = Arg.(non_empty & pos_all ocaml_file [] & info [] ~docv:"FILE")

let run_tc verbose load_path file =
  let load_path =
    List.fold_left
      (fun acc f ->
        let dir = Filename.dirname f in
        if not (List.mem dir acc) then dir :: acc else acc)
      load_path file
  in
  let b = Tc.run { verbose; load_path } file in
  if not b then exit 125 else ()

let run_dumpast load_path file =
  let load_path =
    List.fold_left
      (fun acc f ->
        let dir = Filename.dirname f in
        if not (List.mem dir acc) then dir :: acc else acc)
      load_path file
  in
  Dumpast.run load_path file

let dumpast =
  let doc = "Gospel dump ast." in
  (Term.(const run_dumpast $ load_path $ files), Term.info "dumpast" ~doc)

let tc =
  let doc = "Gospel type-checker." in
  (Term.(const run_tc $ verbose $ load_path $ files), Term.info "check" ~doc)

let pps =
  let doc = "Gospel preprocessor." in
  (Term.(const Pps.run $ files), Term.info "pps" ~doc)

let wc =
  let doc = "Gospel line count." in
  (Term.(const Wc.run $ files), Term.info "cloc" ~doc)

let usage_cmd =
  let doc = "Gospel command line tool." in
  ( Term.(ret (const (`Help (`Auto, None)))),
    Term.info "gospel" ~doc ~version:"gospel version %%VERSION%%" )

let () =
  let commands = [ tc; wc; pps; dumpast ] in
  Term.(exit @@ eval_choice usage_cmd commands)
