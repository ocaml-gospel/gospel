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
   named parameter not specified in function header *)

(* {gospel_expected|
   [125] File "labeled_arg8.mli", line 12, characters 10-11:
         12 | (*@ r = f x y z *)
                        ^
         Error: Type checking error: parameter does not match with val type.
   |gospel_expected} *)
