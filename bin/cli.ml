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

let verbose =
  let doc = "Print all intermediate forms." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let load_path =
  let doc = "Include directory in load path." in
  Arg.(value & opt_all dir [] & info [ "L"; "load-path" ] ~doc ~docv:"DIR")

let intfs =
  let doc = "File to be processed, expect a .mli or a .gospel file" in
  Arg.(non_empty & pos_all test_intf [] & info [] ~doc ~docv:"FILE")

let files =
  let doc = "File to be processed, expect a .mli or a .ml file" in
  Arg.(non_empty & pos_all test_file [] & info [] ~doc ~docv:"FILE")

let run_check verbose load_path file =
  let load_path =
    List.fold_left
      (fun acc f ->
        let dir = Filename.dirname f in
        if not (List.mem dir acc) then dir :: acc else acc)
      load_path file
  in
  let b = Check.run { verbose; load_path } file in
  if not b then exit 125 else ()

let run_dumpast load_path file =
  let load_path =
    List.fold_left
      (fun acc f ->
        let dir = Filename.dirname f in
        if not (List.mem dir acc) then dir :: acc else acc)
      load_path file
  in
  let b = Dumpast.run { load_path } file in
  if not b then exit 125 else ()

let dumpast =
  let doc = "Gospel dump ast." in
  let info = Cmd.info "dumpast" ~doc in
  let term = Term.(const run_dumpast $ load_path $ intfs) in
  Cmd.v info term

let tc =
  let doc = "Gospel type-checker." in
  let info = Cmd.info "check" ~doc in
  let term = Term.(const run_check $ verbose $ load_path $ intfs) in
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
  let commands = Cmd.group info [ tc; wc; pps; dumpast ] in
  Stdlib.exit (Cmd.eval commands)
