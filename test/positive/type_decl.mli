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
type 'a t2
type ('a, 'b) t3
type test = T1 | T2 | T3 | T4
type test2 = T5 of int
type 'a test3 = T6 of 'a
type 'a test4 = T7 of 'a | T8 of int
type 'a test5 = T9 of int * 'a
type ('a, 'b) test6 = T10 of int * 'a | T11 of 'b * 'a
type ('x, 'g) test7 = T12 of 'g * int
type t10 = { x : int }
type t11 = { x : int; mutable y : float }
type 'a t12 = { x : 'a; y : 'a * int }
type t13 = T13 of { x : int }
type 'a t14 = T14 of { x : 'a; y : int }
type 'a t15 = T151 of { x : 'a; y : int } | T152 of int * 'a
type 'a t16 = int
type 'a t17 = 'a

type ('a, 'b, 'c) t18 =
  | C1 of 'a * 'b
  | C2 of { i : int; a : 'a }
  | C3
  | C4 of { i : int; a : 'a }
  | C5 of { c : 'c }

type 'a t19 = C of 'a
and 'b t20 = 'b t19 = C of 'b

type 'a t21 = 'a t22 = C of 'a
and 'b t22 = C of 'b

type t23 = u
and u = C of v
and v = t23
