(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int ref -> unit
(*@ pure
    modifies x
    let () = f x *)

(* {gospel_expected|
[1] File "pure_cant_modify.mli", line 11, characters 0-67:
    11 | val f : int ref -> unit
    12 | (*@ pure
    13 |     modifies x
    14 |     let () = f x *)
    Error: A function annotated as pure cannot modify a variable.
    
|gospel_expected} *)
