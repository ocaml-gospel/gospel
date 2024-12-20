(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val f : int -> int -> int
(*@ r = f x y
    requires x.v > 0
    requires y.v + 2 < 0
    requires x.v + 1 < 0
    ensures r.v = x.v + y.v
    ensures r.v > 2
    ensures r.v = 3 *)

exception X
exception Y of int

val f : int -> int -> int
(*@ r = f x y
    raises X -> x.v = 2
    raises Y i -> i.v = y.v + 2 -3 / x.v
    requires x.v > 0
    requires y.v + 2 < 0
    requires x.v + 1 < 0
    ensures r.v = x.v + y.v
    ensures r.v > 2
    ensures r.v = 3 *)
