(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { x : integer; x : integer } *)

(* {gospel_expected|
[1] File "duplicate_label1.mli", line 11, characters 28-39:
    11 | (*@ type t = { x : integer; x : integer } *)
                                     ^^^^^^^^^^^
    Error: Two labels are named x
    
|gospel_expected} *)
