(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type 'a t2 = C2 of 'a | C3 of bool | C4 of int * 'a

(*@ function gnr: 'a *)

(*@ function g (x y:'a) (i: int): 'a *)

(*@ function f (x: 'a t2): 'a =
    match x with
    | C2 x -> true
    | C3 b -> gnr
    | C4 (i,x) -> g x x i
*)

(* {gospel_expected|
   [125] File "any_not_bool.mli", line 21, characters 18-19:
         21 |     | C4 (i,x) -> g x x i
                                ^
         Error: This term has type 'a but a term was expected of type bool.
   |gospel_expected} *)
