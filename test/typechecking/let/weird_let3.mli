(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function ( let+ ) (x : integer) (f : integer -> integer) : integer *)

(*@ axiom test : let+ x = 0 in x + x *)

(* {gospel_expected|
[1] File "weird_let3.mli", line 13, characters 17-36:
    13 | (*@ axiom test : let+ x = 0 in x + x *)
                          ^^^^^^^^^^^^^^^^^^^
    Error: Mismatch between type prop and type integer
    
|gospel_expected} *)
