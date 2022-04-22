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

type config = { load_path : string list }

let path2module p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

let type_check load_path name sigs =
  let md = init_muc name in
  let penv =
    path2module name |> Utils.Sstr.singleton |> Typing.penv load_path
  in
  let md = List.fold_left (Typing.type_sig_item penv) md sigs in
  wrap_up_muc md

let run_file { load_path } file =
  try
    let ocaml = parse_ocaml file in
    let module_nm = path2module file in
    let sigs = parse_gospel ~filename:file ocaml module_nm in
    let file = type_check load_path file sigs in
    Fmt.pf Fmt.stdout "%s\n" (Tast.show_signature file.fl_sigs);
    true
  with W.Error e ->
    Fmt.epr "%a@." W.pp e;
    false

let run config files =
  List.fold_right (fun file b -> run_file config file && b) files true
