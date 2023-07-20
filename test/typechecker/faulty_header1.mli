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
(*@ r = f x y *)

(* {gospel_expected|
   [125] File "faulty_header1.mli", line 12, characters 8-9:
         12 | (*@ r = f x y *)
                      ^
         Error: Type checking error: too few parameters.
   |gospel_expected} *)
