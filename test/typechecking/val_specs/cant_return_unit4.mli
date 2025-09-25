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
(*@ () = f x
    pure
    preserves x *)

(* {gospel_expected|
[1] File "cant_return_unit4.mli", line 12, characters 3-38:
    12 | ... () = f x
    13 |     pure
    14 |     preserves x ..
    Error: This function has no listed side effects, it cannot return unit
    
|gospel_expected} *)
