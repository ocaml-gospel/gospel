(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : x:('a -> 'b -> 'c) -> y:'a -> 'b -> 'c
(*@ [b:integer],[a:'a] = f ~x [w:int] ~y [p:integer] z *)

(* ERROR:
   Line 12
   no return value in function specification header
   add a new return var in line 12 *)

(* {gospel_expected|
   [125] File "no_return.mli", line 12, characters 25-26:
         12 | (*@ [b:integer],[a:'a] = f ~x [w:int] ~y [p:integer] z *)
                                       ^
         Error: Type checking error: too few returned values.
   |gospel_expected} *)
