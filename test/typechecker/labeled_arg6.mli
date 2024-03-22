(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : y:int -> int -> int
(*@ r = f ?y x*)

(* ERROR:
   the first parameter is named but is defined as optional in spec header *)

(* {gospel_expected|
   [125] File "labeled_arg6.mli", line 12, characters 11-12:
         12 | (*@ r = f ?y x*)
                         ^
         Error: Type checking error: parameter does not match with val type.
   |gospel_expected} *)
