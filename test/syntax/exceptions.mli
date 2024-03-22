(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

exception E
exception E1 of int
exception E2 of int * int
exception E2pair of (int * int)
exception E3 of int list
exception E4 of int * int list
exception E5 of (int -> int)
exception E6 of (int -> float -> bool list)
exception E7 of { x : int }
exception E8 of { x : (int -> float) }
exception E9 of { x : int; y : float }
exception E10 of { x : (int -> int -> float); y : float; z : bool }
exception E11 of char

[@@@gospel "val id : 'a -> 'a"]

(*@ function integer_of_int (x:int): integer *)
(*@ function int_of_integer (x:integer): int *)

val f : 'a -> 'a
(*@ x = f y
    raises E1 x -> integer_of_int x = 1 | E1 _ -> false
    raises E2 (_, _)
    raises E2 (_, _) -> true
    raises E2 (_, _) -> true
    raises E2pair _ -> true
    raises E2pair (x, y) -> true
    raises E2pair (_, _) -> true
    raises E2pair z -> true
    raises E3 l -> (match l with
                   | [] -> false
                   | y :: ys -> integer_of_int y = 2)
         | E4 (i,l) -> match l with
                   | [] -> true
                   | y :: ys -> y = i
    raises E5 f -> integer_of_int (f (int_of_integer 3)) = 4
    raises E11 'a' -> true
    raises E11 ('a' | 'b') -> true
    raises E11 ('c'..'z') -> true *)
