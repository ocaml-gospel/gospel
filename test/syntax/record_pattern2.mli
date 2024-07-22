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
   [125] File "record_pattern2.mli", line 15, characters 6-9:
         15 |     | {y} -> y
                    ^^^
         Error: Unbound record field: y.
   |gospel_expected} *)
