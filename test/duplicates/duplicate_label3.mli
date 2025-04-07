(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { x : integer; y : integer; x : integer } *)

(* {gospel_expected|
[1] File "duplicate_label3.mli", line 11, characters 41-52:
    11 | (*@ type t = { x : integer; y : integer; x : integer } *)
                                                  ^^^^^^^^^^^
    Error: Two labels are named x
    
|gospel_expected} *)
