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
open Bin_utils
module W = Warnings

(** Directory in which compiled Gospel files will be placed. *)
let comp_dir = "_gospel"

(** [error msg file] receives an error message template [msg] that expects the
    name of a file and exits the program with exit code [1] and prints [msg]
    where the name of the file is [file]. *)
let error msg file =
  Fmt.epr msg file;
  exit 1

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

let path2module p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

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
    let () = Ident.Tag.set_project_name file in
    let mods =
      if Filename.extension file = gospel_ext then read_gospel_file file
      else check_file ~comp_dir ~env file
    in
    let module_nm = path2module file in
    let id = Ident.mk_id module_nm Location.none in
    Namespace.add_mod env id mods
  in
  (* Un-marshal the gospel standard library that is created in compile time. *)
  let stdlib : Namespace.mod_defs =
    (* At compile time the [%blob] annotation is replaced with the raw string
       of the marshalled gospel standard library. *)
    Marshal.from_string [%blob "../stdlib/gospelstdlib.gospel"] 0
  in

  let env = Namespace.init_env stdlib in
  let _ =
    try List.fold_left check env files
    with Warnings.Error e ->
      Fmt.epr "%a@." W.pp e;
      exit 1
  in
  ()
