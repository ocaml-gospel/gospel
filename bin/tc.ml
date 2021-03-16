(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ppxlib
open Gospel
open Tmodule
open Parser_frontend

type config = {
  bench_mode : bool;
  print_intermediate : bool;
  print_parsed : bool;
  parse_only : bool;
  parse_ocaml_only : bool;
  load_path : string list;
}

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

let run_bench config files =
  let ok, error = (ref 0, ref 0) in
  let parse f =
    try
      let ocaml = parse_ocaml f in
      let module_nm = path2module f in
      let sigs = parse_gospel ocaml module_nm in
      pp fmt "parse OK - ";
      incr ok;
      if config.parse_only then raise Exit;
      ignore (type_check config.load_path f sigs : file);
      pp fmt "type-check OK - ";
      raise Exit
    with
    | Exit -> Format.fprintf fmt "%s@\n" f
    | _ ->
        incr error;
        pp fmt " *** ERROR *** - %s@\n" f
  in
  List.iter parse files;
  pp fmt "@[@\n Parsing OK: %d  -  ERRORs: %d@\n@]@." !ok !error

let run_file config file =
  try
    let ocaml = parse_ocaml file in
    if config.print_intermediate then (
      pp fmt "@[@\n*******************************@]@.";
      pp fmt "@[********** Parsed file ********@]@.";
      pp fmt "@[*******************************@]@.";
      pp fmt "@[%a@]@." Opprintast.signature ocaml );
    if config.parse_ocaml_only then raise Exit;

    let module_nm = path2module file in
    let sigs = parse_gospel ocaml module_nm in
    if config.print_intermediate || config.print_parsed then (
      pp fmt "@[@\n*******************************@]@.";
      pp fmt "@[****** GOSPEL translation *****@]@.";
      pp fmt "@[*******************************@]@.";
      pp fmt "@[%a@]@." Upretty_printer.s_signature sigs );
    if config.parse_only then raise Exit;

    let file = type_check config.load_path file sigs in
    pp fmt "@[@\n*******************************@]@.";
    pp fmt "@[********* Typed GOSPEL ********@]@.";
    pp fmt "@[*******************************@]@.";
    pp fmt "@[%a@]@." print_file file;
    pp fmt "@[@\n*** OK ***@\n@]@."
  with
  | Exit -> ()
  | Not_found ->
      let open Format in
      eprintf "File %s not found.@\nLoad path: @\n%a@\n@." file
        (pp_print_list ~pp_sep:pp_print_newline pp_print_string)
        config.load_path
  | e -> Location.report_exception Format.err_formatter e

let run config files =
  if config.bench_mode then run_bench config files
  else List.iter (run_file config) files
