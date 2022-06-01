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
    | {y} -> y *)

(* ERROR:
   Line 15
   no record with field y
   replace y by x in line 15 *)

(* {gospel_expected|
   [125] File "t34.mli", line 15, characters 7-8:
         15 |     | {y} -> y *)
                     ^
         Error: Symbol y not found.
   |gospel_expected} *)
