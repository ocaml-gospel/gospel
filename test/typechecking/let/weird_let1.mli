(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function ( let* ) (x : integer) (y : integer) : integer *)

(*@ axiom weird : let* y = 0 in True *)

(* {gospel_expected|
[1] File "weird_let1.mli", line 13, characters 32-36:
    13 | (*@ axiom weird : let* y = 0 in True *)
                                         ^^^^
    Error: Mismatch between type integer and type 'b -> 'a
    
|gospel_expected} *)
