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

let file s = List.mem (Filename.extension s) [ ".mli"; ".ml" ]
let intf s = List.mem (Filename.extension s) [ ".mli"; ".gospel" ]

let test test =
  let parse s =
    match Sys.file_exists s with
    | true ->
        if test s then `Ok s
        else `Error (Printf.sprintf "don't know what to do with %s" s)
    | false -> `Error (Printf.sprintf "Error: `%s' not found" s)
  in
  (parse, Format.pp_print_string)

let test_intf = test intf
let test_file = test file

let intfs =
  let doc = "File to be processed, expect a .mli or a .gospel file" in
  Arg.(non_empty & pos_all test_intf [] & info [] ~doc ~docv:"FILE")

let files =
  let doc = "File to be processed, expect a .mli or a .ml file" in
  Arg.(non_empty & pos_all test_file [] & info [] ~doc ~docv:"FILE")

let run_check file = List.iter Check.run file

let tc =
  let doc = "Gospel type-checker (Experimental)." in
  let info = Cmd.info "check" ~doc in
  let term = Term.(const run_check $ intfs) in
  Cmd.v info term

let pps =
  let doc = "Gospel preprocessor." in
  let info = Cmd.info "pps" ~doc in
  let term = Term.(const Pps.run $ files) in
  Cmd.v info term

let wc =
  let doc = "Gospel line count." in
  let info = Cmd.info "cloc" ~doc in
  let term = Term.(const Cloc.run $ intfs) in
  Cmd.v info term

let () =
  let doc = "Gospel command line tool."
  and version =
    Printf.sprintf "gospel version: %s"
      (match Build_info.V1.version () with
      | None -> "n/a"
      | Some v -> Build_info.V1.Version.to_string v)
  in
  let info = Cmd.info "gospel" ~doc ~version in
  let commands = Cmd.group info [ tc; wc; pps ] in
  Stdlib.exit (Cmd.eval commands)
