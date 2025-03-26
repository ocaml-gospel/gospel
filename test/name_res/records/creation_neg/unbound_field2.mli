(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { y : integer } *)

(*@ function test : t = { x = 0 } *)

(* {gospel_expected|
[1] File "unbound_field2.mli", line 13, characters 24-33:
    13 | (*@ function test : t = { x = 0 } *)
                                 ^^^^^^^^^
    Error: No record found with the provided labels
    
|gospel_expected} *)
