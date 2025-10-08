(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function ( let* ) (x : integer) (f : prop -> integer) : integer *)

(*@ axiom wrong : let* y = 0 in y = y *)

(* {gospel_expected|
[1] File "weird_let2.mli", line 13, characters 32-37:
    13 | (*@ axiom wrong : let* y = 0 in y = y *)
                                         ^^^^^
    Error: Mismatch between type integer and type prop
    
|gospel_expected} *)
