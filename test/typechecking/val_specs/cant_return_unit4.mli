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
    preserves x
    let () = f x *)

(* {gospel_expected|
[1] File "cant_return_unit4.mli", line 12, characters 3-42:
    12 | ... pure
    13 |     preserves x
    14 |     let () = f x ..
    Error: This function has no listed side effects, it cannot return unit
    
|gospel_expected} *)
