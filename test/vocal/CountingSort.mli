(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate k_values (k: int) (a: int sequence) =
      forall i. 0 <= i < Sequence.length a -> 0 <= a[i].v < k.v *)

(*@ predicate sorted (a: int sequence) =
      forall i j. 0 <= i <= j < Sequence.length a -> a[i].v <= a[j].v *)

val counting_sort : int -> int array -> int array -> unit
(*@ counting_sort k a b
      requires 0 < k.v
      requires k_values k a.array_content
      requires Sequence.length a.array_content = Sequence.length b.array_content
      modifies b.array_content
      ensures  sorted b.array_content
      ensures  Sequence.permut a.array_content b.array_content *)

val in_place_counting_sort : int -> int array -> unit
(*@ in_place_counting_sort k a
      requires 0 < k.v
      requires k_values k a.array_content
      modifies a.array_content
      ensures  sorted a.array_content
      ensures  Sequence.permut (old a.array_content) a.array_content *)
