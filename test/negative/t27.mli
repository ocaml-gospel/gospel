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

(* EXPECTED
   [125] File "t27.mli", line 12, characters 22-23:
         Error: Type checking error: too few parameters.
*)
