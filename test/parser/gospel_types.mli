(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Positive tests for the definition of Gospel types*)

(* Abstract types *)
(*@ type t1 *)
(*@ type 'a t2 *)
(*@ type ('a, 'b) t3 *)

(* Type aliases *)
(*@ type t4 = t1 *)
(*@ type 'a t5 = 'a t2 *)
(*@ type ('a, 'b) t6 = ('a, 'b) t3 *)

(*@ type t7 = integer -> integer *)
(*@ type t8 = integer -> integer -> integer *)
(*@ type t9 = integer -> (integer -> integer) *)
(*@ type t9 = (integer -> integer) -> integer *)

(*@ type t10 = integer * integer *)
(*@ type t11 = integer * integer * integer *)
(*@ type t12 = integer * (integer * integer) *)
(*@ type t13 = (integer * integer) * integer *)

(*@ type 'a t14 =
      'a * 'a ->
      (integer -> integer * 'a t5) ->
      (('a, 'a) t6 * 'a t5 -> 'a t5) *)

(*@ type t15 = integer t2 *)
(*@ type 'a t16 = (integer, 'a) t3 *)
(*@ type 'a t17 = ('a, integer) t3 *)
(*@ type t18 = (integer, integer) t3 *)

(* Record types *)
(*@ type t19 = { f1 : t1 } *)
(*@ type t20 = { f1 : t1; } *)
(*@ type t21 = { f2 : t1; f3 : t1 } *)
(*@ type t22 = { f2 : t1; f3 : t1; } *)

(*@ type t23 = { f1 : t1 -> t1 } *)
(*@ type t24 = { f1 : t1 * t1 } *)
(*@ type t25 = { f2 : t1; f3 : t1 -> t1 } *)
(*@ type t26 = { f2 : t1; f3 : t1 * t1 } *)

(*@ type 'a t27 = { f4 : 'a } *)
(*@ type 'a t28 = { f4 : 'a; } *)
(*@ type 'a t29 = { f5 : 'a; f6 : 'a } *)
(*@ type 'a t30 = { f5 : 'a; f6 : 'a; } *)

(*@ type 'a t31 = { f7 : 'a t2 } *)
(*@ type 'a t32 = { f8 : 'a t2; f9 : 'a t2 } *)

(*@ type ('a, 'b) t33 = { f10 : ('a, 'b) t3 } *)
(*@ type ('a, 'b) t34 = { f11 : ('a, 'b) t3; f12 : ('a, 'b) t3 } *)
