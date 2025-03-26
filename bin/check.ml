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
open Parser_frontend
open Checker

let path2module p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

let run file =
  let ocaml = parse_ocaml file in
  let module_nm = path2module file in
  try
    let sigs = parse_gospel ~add_std:false ~filename:file ocaml module_nm in
    let _ = signatures sigs in
    ()
  with Warnings.Error e -> Fmt.epr "%a@." W.pp e
