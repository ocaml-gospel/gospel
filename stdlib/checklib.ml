(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module W = Gospel.Warnings
(** Simple script to type check the standard library and generate a [.gospel]
    file for it. *)

let () =
  let _ =
    try Bin_utils.check_file "gospelstdlib.mli"
    with W.Error e ->
      Fmt.epr "%a@." W.pp e;
      exit 1
  in
  ()
