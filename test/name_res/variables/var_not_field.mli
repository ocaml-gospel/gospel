(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { field : integer } *)

(*@ axiom test : field *)

(* {gospel_expected|
[1] File "var_not_field.mli", line 13, characters 17-22:
    13 | (*@ axiom test : field *)
                          ^^^^^
    Error: Unbound value field
    
|gospel_expected} *)
