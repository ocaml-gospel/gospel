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
    | {x=x;y=x} -> x *)

(* ERROR:
   Line 13
   variable x is duplicated in pattern of line 15
   replace one of the variables in line 15 by other name *)
