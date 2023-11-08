(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate k_values (k: int) (a: int array) =
      forall i. 0 <= i < Array.length a -> 0 <= a.(i) < k *)

(*@ predicate sorted (a: int array) =
      forall i j. 0 <= i <= j < Array.length a -> a.(i) <= a.(j) *)

val counting_sort : int -> int array -> int array -> unit
(*@ counting_sort k a b
      requires 0 < k
      requires k_values k a
      requires Array.length a = Array.length b
      modifies b
      ensures  sorted b
      ensures  Array.permut a b *)

val in_place_counting_sort : int -> int array -> unit
(*@ in_place_counting_sort k a
      requires 0 < k
      requires k_values k a
      modifies a
      ensures  sorted a
      ensures  Array.permut (old a) a *)
