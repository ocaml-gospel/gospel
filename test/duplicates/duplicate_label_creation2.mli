(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { x : integer; y : integer } *)

(*@ function f : t = { y = 0; x = 0; x = 0 }*)
(* {gospel_expected|
[1] File "duplicate_label_creation2.mli", line 13, characters 21-44:
    13 | (*@ function f : t = { y = 0; x = 0; x = 0 }*)
                              ^^^^^^^^^^^^^^^^^^^^^^^
    Error: No record found with the provided labels
    
|gospel_expected} *)
