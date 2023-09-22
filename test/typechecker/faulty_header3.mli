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
(*@ r = f ~x [z:int] ~y z *)

(* {gospel_expected|
   [125] File "faulty_header3.mli", line 12, characters 24-25:
         12 | (*@ r = f ~x [z:int] ~y z *)
                                      ^
         Error: The variable z is duplicated in this pattern.
   |gospel_expected} *)
