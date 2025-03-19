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
open Typing
open Parser_frontend

let gospel_ext = ".gospel"
let path2file p = Filename.basename p |> Filename.chop_extension

(** [check_file file] parses and type checks [file] and creates a corresponding
    [.gospel] file in the directory [comp_dir].
    @raise Warnings.Error if there is a parsing or typing error in [file]. *)
let check_file ?(comp_dir = "") ?(env = Namespace.empty_env) file =
  (* If the compilation directory is non empty and does not end with "/", add
     it. *)
  let needs_dash =
    comp_dir <> "" && comp_dir.[String.length comp_dir - 1] <> '/'
  in
  let comp_dir = if needs_dash then comp_dir ^ "/" else comp_dir in
  let ocaml = parse_ocaml file in
  let module_nm = path2file file in
  let sigs = parse_gospel ~filename:file ocaml in
  let _, mods = signatures env sigs in
  (* Stores the Gospel definitions in [file] in a [.gospel] file in the
     [_gospel] directory.

     Issue : If two files have the same name but are in different
     sub-directories, this will write them to the same file. *)
  let comp =
    open_out_bin (Format.sprintf "%s%s%s" comp_dir module_nm gospel_ext)
  in
  Marshal.to_channel comp mods [];
  close_out comp;
  mods
