(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function n : integer *)

(*@ axiom test : n n *)

(* {gospel_expected|
[1] File "not_fun2.mli", line 13, characters 17-18:
    13 | (*@ axiom test : n n *)
                          ^
    Error: Mismatch between type 'b -> 'a and type integer
    
|gospel_expected} *)
