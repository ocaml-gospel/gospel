(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t = { x : int }

(*@ function f (x: t): int =
    match x with
    | {x=y} -> x *)

(* ERROR:
   Line 15
   x is of type t and not int
   replace x by y in the term of line 15 *)

(* {gospel_expected|
   [125] File "t33.mli", line 14, characters 4-33:
         14 | ....match x with
         15 |     | {x=y} -> x...
         Error: This term has type `t' but a term was expected of type `int'.
   |gospel_expected} *)
