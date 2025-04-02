(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax : ax *)
(* {gospel_expected|
[1] File "axiom_name_invalid.mli", line 11, characters 15-17:
    11 | (*@ axiom ax : ax *)
                        ^^
    Error: Unbound value ax
    
|gospel_expected} *)
