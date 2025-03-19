(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)
(** Contains the definitions necessary for type checking the standard library
    and for the Gospel type checker. *)

open Gospel

val gospel_ext : string
(** The extension that compiled gospel files have. *)

val check_file :
  ?comp_dir:string -> ?env:Namespace.env -> string -> Namespace.mod_defs
(** [check_file comp_dir env file] receives an [.mli] file name and type checks
    it under [env]. If the file is correctly type checked, we create a file in
    directory [comp_dir] with the name [file] where [.mli] has been replaced
    with [gospel_ext].
    @raise Warnings.Error
      If there is a parsing or type checking error within [file]. *)
