(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type 'a t1 = { x : int; y : 'a }
type ('a, 'b) t2 = 'a t1

(*@ function f (x: 'a t1): ('b,int) t2 =
    match x with
    | {x;y} -> {x;y} *)

(* ERROR:
   Line 15
   cannot match ('b,int) t2 with 'a t1
   replace 'b by 'a in the return type of function f *)

(* {gospel_expected|
   [125] File "t38.mli", line 15, characters 4-37:
         15 | ....match x with
         16 |     | {x;y} -> {x;y}...
         Error: This term has type `'a t1' but a term was expected of type `'b t1'.
   |gospel_expected} *)
