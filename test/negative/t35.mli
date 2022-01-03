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
