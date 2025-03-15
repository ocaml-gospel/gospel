(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val run : string list -> unit
(** [run files] receives a list of file names that are either [.mli] (OCaml
    interfaces) or [.gospel] (compiled Gospel modules). Each [.mli] file is type
    checked in a context where all files previous to it are visible in the form
    of a Gospel module. Consequently the order in which files appear in the list
    matter: if a file [f1] depends on [f2], [f2] should appear before [f1].

    This function fails with an exit code [1] if:

    - The [files] list contains duplicates.
    - Any of the files do not have the extension [.gospel] or [.mli].
    - Any of the [.mli] files do not parse or type check.
    - Any of the [.gospel] files are not compatible with the current version of
      Gospel. *)
