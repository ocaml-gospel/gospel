(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type 'a t = { x : int; y : 'a }

(*@ function f (x: 'a t): int =
    match x with
    | { x = x; y = x } -> x *)

(* {gospel_expected|
   [125] File "record_pattern4.mli", line 15, characters 6-22:
         15 |     | { x = x; y = x } -> x *)
                    ^^^^^^^^^^^^^^^^
         Error: The variable x is duplicated in this pattern.
   |gospel_expected} *)
