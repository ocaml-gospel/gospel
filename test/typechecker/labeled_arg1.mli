(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : x:int -> int -> int
(*@ r = f x y*)

(* ERROR:
   Line 12
   first parameter is named
   add ~ before x in line 12 *)

(* {gospel_expected|
   [125] File "labeled_arg1.mli", line 12, characters 10-11:
         12 | (*@ r = f x y*)
                        ^
         Error: Type checking error: parameter does not match with val type.
   |gospel_expected} *)
