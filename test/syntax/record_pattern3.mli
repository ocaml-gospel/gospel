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
    | { x; z } -> x
*)

(* {gospel_expected|
   [125] File "record_pattern3.mli", line 15, characters 11-12:
         15 |     | { x; z } -> x
                         ^
         Error: Symbol z not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
