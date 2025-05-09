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
(*@ preserves x
    let () = f x *)

(* {gospel_expected|
[1] File "cant_return_unit2.mli", line 12, characters 3-33:
    12 | ... preserves x
    13 |     let () = f x ..
    Error: This function has no listed side effects, it cannot return unit
    
|gospel_expected} *)
