(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t1 *)

(*@ type t2 = t1 *)

(*@ predicate test1 (x : t1) (y : t2) = x = y /\ y = x *)
(*@ predicate test2 (s1 : t1 sequence) (s2 : t2 sequence) =
      s1 = s2 /\ s1[0] = s2[0] *)

(*@ type 'a t3 *)
(*@ type 'a t4 = 'a t3 *)

(*@ predicate test3 (x : 'a t3) (y : 'a t4) = x = y *)
(*@ predicate test4 (x : integer t3 t4) (y : integer t4 t3) = x = y *)
(*@ predicate test5 (f1 : t1 -> t2) (f2 : 'a t3 -> 'a t4) =
      forall x : t1.
      forall y : 'a t4.
        f1 x = x /\ f2 y = y *)

(*@ type t5 = t2 *)
(*@ type 'a t6 = 'a t4 *)

(*@ axiom test6 :
      forall f1 : (t5 -> t5).
      forall f2 : ('a t6 -> 'a t6).
        test5 f1 f2  *)

(*@ type ('a, 'b) t7 = t5 * 'a t6 * 'b t6 *)

(*@ predicate test7 (x : ('a, 'b) t7) (y : t1 * 'a t3 * 'b t4) = x = y *)

(*@ type ('a, 'b) t8 = { f : ('a, 'b) t7 } *)
(*@ type 'a t9 = ('a, 'a) t8 *)

(*@ axiom test8 :
      forall r1 : ('a, 'a) t8.
      forall r2 : 'a t9.
      forall y : ('a, 'a) t7.
        r1.f = r2.f = y *)

(*@ type t10 = t1 -> t2 *)

(*@ predicate test9 (f : t10) (x : t1) (y : t2) =
      f x = f y *)

(*@ type 'a t11 = 'a *)

(*@ predicate test10 (x : 'a t11) (y : 'a) = x = y *)
