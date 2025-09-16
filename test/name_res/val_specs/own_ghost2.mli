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
(*@ z = f [x : integer] y
    modifies x *)

(* {gospel_expected|
[1] File "own_ghost2.mli", line 13, characters 13-14:
    13 |     modifies x *)
                      ^
    Error: Unbound value x
    
|gospel_expected} *)
