(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Gospel
open Tmodule
open Parser_frontend
module W = Gospel.Warnings

type config = { verbose : bool; load_path : string list }

let fmt = Format.std_formatter
let pp = Format.fprintf

let path2module p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

let type_check load_path name sigs =
  let md = init_muc name in
  let penv =
    path2module name |> Utils.Sstr.singleton |> Typing.penv load_path
  in
  let md = List.fold_left (Typing.type_sig_item penv) md sigs in
  md

let run_file config file =
  try
    let md =
      if String.equal ".gospel" (Filename.extension file) then
        read_gospel_file file
      else
        let ocaml = parse_ocaml file in
        if config.verbose then (
          pp fmt "@[@\n*******************************@]@.";
          pp fmt "@[********** Parsed file ********@]@.";
          pp fmt "@[*******************************@]@.";
          pp fmt "@[%a@]@." Opprintast.signature ocaml);

        let module_nm = path2module file in
        let sigs = parse_gospel ~filename:file ocaml module_nm in
        if config.verbose then (
          pp fmt "@[@\n*******************************@]@.";
          pp fmt "@[****** GOSPEL translation *****@]@.";
          pp fmt "@[*******************************@]@.";
          pp fmt "@[%a@]@." Upretty_printer.s_signature sigs);
        type_check config.load_path file sigs
    in
    let file = wrap_up_muc md in
    if config.verbose then (
      pp fmt "@[@\n*******************************@]@.";
      pp fmt "@[********* Typed GOSPEL ********@]@.";
      pp fmt "@[*******************************@]@.";
      pp fmt "@[%a@]@." print_file file);
    write_gospel_file md;
    true
  with W.Error e ->
    let bt = Printexc.get_backtrace () in
    Fmt.epr "%a@." W.pp e;
    (match Sys.getenv_opt "GOSPELDEBUG" with
    | Some _ -> Printf.fprintf stderr "\nBacktrace of the error:\n%s" bt
    | None -> ());
    false

let run config files =
  List.fold_right (fun file b -> run_file config file && b) files true
