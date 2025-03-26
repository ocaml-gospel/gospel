(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t1 = { x : integer } *)
(*@ type t2 = { y : integer } *)

(*@ function t : t1 = { x = 0; y = 0 } *)

(* {gospel_expected|
[1] File "incompatible_fields1.mli", line 14, characters 22-38:
    14 | (*@ function t : t1 = { x = 0; y = 0 } *)
                               ^^^^^^^^^^^^^^^^
    Error: No record found with the provided labels
    
|gospel_expected} *)
