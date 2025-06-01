(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Gospel_checker

module W = Warnings
(** Simple script to type check the Gospel and OCaml standard library and
    generate a [.gospel] file for it. *)

let () =
  let _ =
    let () = Ident.Tag.set_project_name Ident.stdlib_project in
    let _, defs = Bin_utils.check_file ~verbose:false "gospelstdlib.mli" in
    let env = Namespace.init_env defs in
    let _, defs =
      Bin_utils.check_file ~verbose:false ~env "ocamlprimitives.mli"
    in
    defs
  in
  ()
