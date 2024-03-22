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

(* {gospel_expected|
   [125] File "record_pattern1.mli", line 14, characters 4-33:
         14 | ....match x with
         15 |     | {x=y} -> x...
         Error: This term has type t but a term was expected of type int.
   |gospel_expected} *)
