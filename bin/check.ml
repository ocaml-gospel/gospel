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
open Typing
module W = Warnings

let path2module p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

let check_file file =
  let ocaml = parse_ocaml file in
  let module_nm = path2module file in
  let sigs = parse_gospel ~add_std:false ~filename:file ocaml module_nm in
  let _ = signatures sigs in
  ()

let run l =
  try List.iter check_file l with Warnings.Error e -> Fmt.epr "%a@." W.pp e
