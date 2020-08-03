(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type 'a t = {x:int; y:'a}

(*@ function f (x: t): int =
    match x with
    | {x=x;y=x} -> x *)

(* ERROR:
   Line 13
   type t must have one argument
   add one argument to type t in line 13 *)
