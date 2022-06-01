(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
(*@ r = f x y z w *)

(* ERROR:
   Line 12
   too many parameters in function header
   add one parameter less in line 12 *)

(* {gospel_expected|
   [125] File "t24.mli", line 12, characters 16-17:
         12 | (*@ r = f x y z w *)
                              ^
         Error: Type checking error: parameter do not match with val type.
   |gospel_expected} *)
