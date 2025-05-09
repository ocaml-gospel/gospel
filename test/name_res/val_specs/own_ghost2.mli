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
(*@ modifies x
    let z = f [x : integer] y *)

(* {gospel_expected|
[1] File "own_ghost2.mli", line 12, characters 13-14:
    12 | (*@ modifies x
                      ^
    Error: Unbound value x
    
|gospel_expected} *)
