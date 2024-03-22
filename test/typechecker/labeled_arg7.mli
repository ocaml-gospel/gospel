(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : ?y:int -> int -> int
(*@ r = f y x *)

(* ERROR:
   the first parameter should be marked as optional in spec header *)

(* {gospel_expected|
   [125] File "labeled_arg7.mli", line 12, characters 10-11:
         12 | (*@ r = f y x *)
                        ^
         Error: Type checking error: parameter does not match with val type.
   |gospel_expected} *)
