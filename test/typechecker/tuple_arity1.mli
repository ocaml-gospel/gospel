(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

exception E of (int * int)

(*@ function integer_of_int (x:int): integer *)
(*@ function fst (x: 'a * 'a): 'a *)

val f : 'a -> 'a
(*@ x = f y
    raises E (x,y,z) -> integer_of_int x = 1 *)

(* ERROR:
   Line 18
   Pattern for exception E does not match type
   remove one of the tuple elements in line 18 *)

(* {gospel_expected|
   [125] File "tuple_arity1.mli", line 18, characters 13-20:
         18 |     raises E (x,y,z) -> integer_of_int x = 1 *)
                           ^^^^^^^
         Error: This pattern matches values of type 'a41 * 'a42 * 'a43
                but a pattern was expected which matches values of type int * int.
   |gospel_expected} *)
