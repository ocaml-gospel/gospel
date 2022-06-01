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
    | {x;z} -> x *)

(* ERROR:
   Line 15
   no record with field z
   erase field z in pattern line 15 *)

(* {gospel_expected|
   [125] File "t35.mli", line 15, characters 9-10:
         15 |     | {x;z} -> x *)
                       ^
         Error: Symbol z not found.
   |gospel_expected} *)
