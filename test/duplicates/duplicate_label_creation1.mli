(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { x : integer } *)

(*@ function f : t = { x = 0; x = 0 }*)
(* {gospel_expected|
[1] File "duplicate_label_creation1.mli", line 13, characters 21-37:
    13 | (*@ function f : t = { x = 0; x = 0 }*)
                              ^^^^^^^^^^^^^^^^
    Error: No record found with the provided labels
    
|gospel_expected} *)
