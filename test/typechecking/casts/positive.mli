(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function test1 : 'a sequence = Sequence.empty : 'a sequence *)

(*@ axiom test2 : forall x : 'a sequence. x = x : prop  *)

(*@ axiom test3 :
      forall f : ('a -> 'b -> 'c).
      forall x : 'a.
      forall y : 'b.
      forall z : 'c.
      (((((f : ('a -> 'b -> 'c)) (x : 'a)) : ('b -> 'c)) (y : 'b)) : 'c) = (z : 'c) *)

(*@ function test4 : integer =
      let x = Sequence.empty : 'a sequence in
       0 *)
