(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : x:('a -> 'b -> 'c) -> 'a -> 'b -> 'c
(*@ r = f x y z *)

(* ERROR:
   Line 12
   named parameter not specified in function header
   add ~ before x in line 12 *)

(* {gospel_expected|
   [125] File "t25.mli", line 12, characters 10-11:
         12 | (*@ r = f x y z *)
                        ^
         Error: Type checking error: parameter do not match with val type.
   |gospel_expected} *)
