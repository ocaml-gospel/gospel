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
(*@ f x
    pure
    modifies x *)

(* {gospel_expected|
[1] File "pure_cant_modify.mli", line 11, characters 0-58:
    11 | val f : int ref -> unit
    12 | (*@ f x
    13 |     pure
    14 |     modifies x *)
    Error: A function annotated as pure cannot modify a variable.
    
|gospel_expected} *)
