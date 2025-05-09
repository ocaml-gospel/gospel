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
(*@ consumes x
    let z = f [x : integer] y *)

(* {gospel_expected|
[1] File "own_ghost1.mli", line 12, characters 13-14:
    12 | (*@ consumes x
                      ^
    Error: Unbound value x
    
|gospel_expected} *)
