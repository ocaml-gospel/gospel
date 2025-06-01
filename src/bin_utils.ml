(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Typing
open Parser_frontend

let gospel_ext = ".gospel"
let path2file p = Filename.basename p |> Filename.chop_extension
let fmt = Format.std_formatter
let pp = Format.fprintf

let type_sigs ~verbose env file =
  let module W = Warnings in
  try
    let ocaml = parse_ocaml file in
    let module_nm = path2file file in
    let sigs = parse_gospel ~filename:file ocaml in
    if verbose then (
      pp fmt "@[@\n*******************************@]@.";
      pp fmt "@[********** Parsed file ********@]@.";
      pp fmt "@[*******************************@]@.";
      pp fmt "@[%a@]@." Uast_printer.signature sigs);
    let tast, env = signatures env sigs in
    (module_nm, tast, env)
  with W.Error e ->
    Fmt.epr "%a@." W.pp e;
    exit 1

let error msg file =
  Fmt.epr msg file;
  exit 1

let read_gospel_file f =
  let ic = open_in f in
  (* Note: although this explicit type annotation is not strictly necessary for
     the code to compile, it ensures that [read_gospel_file] can only be used to
     un-marshal values of type [Namespace.mod_defs]. *)
  let defs : Namespace.mod_defs =
    try Marshal.from_channel ic
    with Failure _ ->
      error
        "Error: the compiled gospel file %s is not compatible with the current \
         version of Gospel."
        f
  in
  close_in ic;
  defs

(* Stores the Gospel definitions in [file] in a [.gospel] file in the
     [_gospel] directory.

     Issue : If two files have the same name but are in different
     sub-directories, this will write them to the same file. *)

(** [check_file file] parses and type checks [file] and creates a corresponding
    [.gospel] file in the directory [comp_dir].
    @raise Warnings.Error if there is a parsing or typing error in [file]. *)
let check_file ?(comp_dir = "") ?(env = Namespace.empty_env) ~verbose file =
  let module_nm, tast, mods = type_sigs ~verbose env file in
  let comp =
    open_out_bin (Format.sprintf "%s%s%s" comp_dir module_nm gospel_ext)
  in
  Marshal.to_channel comp mods [];
  close_out comp;
  (tast, mods)
