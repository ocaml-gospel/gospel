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
(*@ r = f ~y y*)

(* ERROR:
   Line 12
   duplicated vars in val header
   remove replace the second y by z in line 12 *)

(* {gospel_expected|
   [125] File "labeled_arg4.mli", line 12, characters 13-14:
         12 | (*@ r = f ~y y*)
                           ^
         Error: The variable y is duplicated in this pattern.
   |gospel_expected} *)
