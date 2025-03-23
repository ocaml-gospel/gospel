(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(* Positive test for Gospel terms *)

(*@ type r = { x : integer; y : integer }*)

(*@ axiom test :
      let t1 = True in
      let t2 = False in
      let t3 = True /\ False in
      let t4 = 4 < 5 < 6 in
      let t5 = 6 > 5 > 4 in
      let t6 = forall x. x = x in
      let t7 = forall x : integer. x = 1 in
      let t8 = forall x y : integer. x = y in
      let t9 = exists x. x = x in
      let t10 = exists x : integer. x = 1 in
      let t11 = exists x y : integer. x = y in
      let t12 = (t1, t2) in
      let t13 = (t1, t2, t3) in
      let t14 = { x = 0; y = 0} in
      let t15 = t14.x in
      let t16 = t14.y in
      let t17 = fun x y -> x + y in
      let t18 = fun (x : integer) (y : integer) -> x + y in
      let t19 = if true then 0 else 1 in
      let t20 = t17 0 0 in
      (t1 : prop)
 *)
