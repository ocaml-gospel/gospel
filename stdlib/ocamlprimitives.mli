(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** This file is used to introduce OCaml primitive types into the scope and pair
    them with their respective [model] fields. *)

type unit
type int
(*@ model : integer *)

type bool
(*@ model : prop *)

type float
type char
(*@ model : char *)

type 'a list
(*@ model : 'a sequence *)

type 'a array
(*@ mutable
    model elems : 'a sequence
    model length : integer
    with arr invariant 0 <= arr.length <= Sequence.length arr.elems *)

type string
(*@ model : char sequence *)

type bytes
(*@ mutable
    model : char sequence *)

type 'a ref
(*@ mutable
    model : 'a *)

type floatarray
