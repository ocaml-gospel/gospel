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

(*@ type test = field *)

(* {gospel_expected|
[1] File "type_not_field.mli", line 13, characters 16-21:
    13 | (*@ type test = field *)
                         ^^^^^
    Error: Unbound type constructor field
    
|gospel_expected} *)
