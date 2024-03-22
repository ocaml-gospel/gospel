(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t1
type 'a t2 = t1

(*@ function f (x: t1): 'a t2 = x *)

type 'c t3 = 'c t2

(*@ function f (x: int t3) : 'a t2 = x *)

type ('a, 'b) t4
type 'a t5 = ('a, 'a) t4

(*@ function f (x: 'a t5): ('a,'a) t4 = x *)
(*@ function f (x: ('a,'a) t4): 'a t5 = x *)

type 'a t6 = { x : int; y : 'a }
type ('a, 'b) t7 = 'a t6

(*@ function f (x: 'a t6) : ('a,'b) t7 =
  match x with
  | {x;y} -> {x;y}
*)
