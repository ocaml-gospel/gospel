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
  wrap_up_muc md

let run_file config file =
  try
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

    let file = type_check config.load_path file sigs in
    if config.verbose then (
      pp fmt "@[@\n*******************************@]@.";
      pp fmt "@[********* Typed GOSPEL ********@]@.";
      pp fmt "@[*******************************@]@.";
      pp fmt "@[%a@]@." print_file file);
    pp fmt "OK\n";
    true
  with W.Error e ->
    Fmt.epr "%a@." W.pp e;
    false

let run config files =
  List.fold_right (fun file b -> run_file config file && b) files true
