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
(*@ r = f ~y x*)

(* ERROR:
   the first parameter is optional but named in spec header *)

(* {gospel_expected|
   [125] File "labeled_arg5.mli", line 12, characters 11-12:
         12 | (*@ r = f ~y x*)
                         ^
         Error: Type checking error: parameter does not match with val type.
   |gospel_expected} *)
