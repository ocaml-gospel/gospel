(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

exception E of (int * int * int)

(*@ function integer_of_int (x:int): integer *)
(*@ function fst (x: 'a * 'a): 'a *)

val f : 'a -> 'a
(*@ x = f y
    raises E (x,y) -> integer_of_int x = 1
*)

(* {gospel_expected|
   [125] File "tuple_arity2.mli", line 18, characters 13-18:
         18 |     raises E (x,y) -> integer_of_int x = 1
                           ^^^^^
         Error: This pattern matches values of type 'a40 * 'a41
                but a pattern was expected which matches values of type
                int * int * int.
   |gospel_expected} *)
