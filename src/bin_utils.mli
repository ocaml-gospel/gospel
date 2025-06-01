(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)
(** Contains utilities for type checking a Gospel file. These are a bit too low
    level for end users and should only be used directly to type check the
    Gospel standard library and other Gospel files that need to be processed at
    compile time. *)

val gospel_ext : string
(** The extension that compiled gospel files have. *)

val error : (string -> unit, Format.formatter, unit) format -> string -> unit
(** [error msg file] receives an error message template [msg] that expects the
    name of a file and exits the program with exit code [1] and prints [msg]
    with the file name [file]. *)

val read_gospel_file : string -> Namespace.mod_defs
(** [read_gospel_file file] un-marshals the contents of the compiled gospel file
    [file] creating a [mod_defs] value with the compiled definitions. This
    function fails if [file] is not a valid [.gospel] file. *)

val check_file :
  ?comp_dir:string ->
  ?env:Namespace.env ->
  verbose:bool ->
  string ->
  Tast.s_signature * Namespace.mod_defs
(** [check_file comp_dir env file] receives an [.mli] file name and type checks
    it under [env]. If the file is correctly type checked, we create a [.gospel]
    file in directory [comp_dir].
    @raise Warnings.Error
      If there is a parsing or type checking error within [file]. *)
