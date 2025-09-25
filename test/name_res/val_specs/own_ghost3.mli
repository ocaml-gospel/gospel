(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int
(*@ z, [x : integer] = f y
    consumes x *)

(* {gospel_expected|
[1] File "own_ghost3.mli", line 13, characters 13-14:
    13 |     consumes x *)
                      ^
    Error: Unbound value x
    
|gospel_expected} *)
