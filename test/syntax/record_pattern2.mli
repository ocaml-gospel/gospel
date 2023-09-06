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
    | {y} -> y
*)

(* TODO better error message: "unknown field name" *)

(* {gospel_expected|
   [125] File "record_pattern2.mli", line 15, characters 7-8:
         15 |     | {y} -> y
                     ^
         Error: Symbol y not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
