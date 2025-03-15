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

(** Directory in which compiled Gospel files will be placed. *)
let comp_dir = "_gospel"

let gospel_ext = ".gospel"

(** [error msg file] receives an error message template [msg] that expects the
    name of a file and exits the program with exit code [1] and prints [msg]
    where the name of the file is [file]. *)
let error msg file =
  Fmt.epr msg file;
  exit 1

(** [read_gospel_file f] un-marshals the contents of the compiled gospel file
    [f] creating a [mod_defs] value with the compiled definitions. This function
    fails if the file [f] is not a valid [.gospel] file. *)
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

(** [check_file file] parses and type checks [file] and creates a corresponding
    [.gospel] file.
    @raise Warnings.Error if there is a parsing or typing error in [file]. *)
let check_file env file =
  let ocaml = parse_ocaml file in
  let module_nm = path2module file in
  let sigs = parse_gospel ~add_std:false ~filename:file ocaml module_nm in
  let _, mods = signatures env sigs in

  (* Stores the Gospel definitions in [file] in a [.gospel] file in the
     [_gospel] directory.

     Issue : If two files have the same name but are in different
     sub-directories, this will write them to the same file. *)
  let comp =
    open_out (Format.sprintf "%s/%s%s" comp_dir module_nm gospel_ext)
  in
  Marshal.to_channel comp env [];
  close_out comp;
  mods

let invalid_extension s =
  let ext = Filename.extension s in
  not (ext = ".mli" || ext = gospel_ext)

(** Checks for errors in the list of file names the user provides to the Gospel
    type checker. No parsing or typechecking is done at this stage. *)
let errors files =
  (* Checks for duplicates in the file list. *)
  Utils.duplicate ( = ) (fun f -> error "The file %s appears twice." f) files;

  (* Checks if all files are OCaml interface files or compiled Gospel modules. *)
  List.iter
    (fun f ->
      if invalid_extension f then
        error
          "Invalid file %s: Files must either be OCaml interface files (.mli) \
           or a compiled gospel module (.gospel). "
          f)
    files

let run files =
  errors files;
  (* Create the compilation directory if it does not already exist. *)
  if not (Sys.file_exists comp_dir) then
    (* The integer argument [0o777] creates the directory with full
       permissions. *)
    Sys.mkdir comp_dir 0o777
  else if not (Sys.is_directory comp_dir) then
    error
      "There exists a non-directory file named \"%s\": since Gospel uses this \
       directory name to store compiled [.gospel] files, please rename or \
       delete this file"
      comp_dir;
  let check env file =
    (* Precondition: this file must have extension [.gospel] or be a valid
       Gospel interface file. *)
    let mods =
      if Filename.extension file = gospel_ext then read_gospel_file file
      else check_file env file
    in
    let module_nm = path2module file in
    let id = Ident.mk_id module_nm Location.none in
    Namespace.add_mod env id mods
  in
  let _ =
    try List.fold_left check Namespace.empty_env files
    with Warnings.Error e ->
      Fmt.epr "%a@." W.pp e;
      exit 1
  in
  ()
