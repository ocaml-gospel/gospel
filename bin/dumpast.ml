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

let path2module p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

let type_check load_path name sigs =
  let md = init_muc name in
  let penv =
    path2module name |> Utils.Sstr.singleton |> Typing.penv load_path
  in
  let md = List.fold_left (Typing.type_sig_item penv) md sigs in
  wrap_up_muc md

let run_dumpast load_path file =
  let ocaml = parse_ocaml_signature file in
  let module_nm = path2module file in
  let sigs = parse_signature_gospel ~filename:file ocaml module_nm in
  let file = type_check load_path file sigs in
  Fmt.pf Fmt.stdout "%s\n" (Tast.show_signature file.fl_sigs)

let run load_path files = List.iter (run_dumpast load_path) files
