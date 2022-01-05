(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** {2 Additional operations on arrays} *)

val binary_search : ('a -> 'a -> int) -> 'a array -> int -> int -> 'a -> int
(** search for value [v] in array [a], between indices [fromi] inclusive and
    [toi] exclusive, using comparison function [cmp]. Returns an index where [v]
    occurs, or raises [Not_found] if no such index exists. *)
(*@ r = binary_search cmp a fromi toi v
      requires Order.is_pre_order cmp
      requires 0 <= fromi <= toi <= Array.length a
      requires forall i j. fromi <= i <= j < toi -> cmp a.(i) a.(j) <= 0
      ensures  fromi <= r < toi && cmp a.(r) v = 0
      raises   Not_found -> forall i. fromi <= i < toi -> cmp a.(i) v <> 0 *)

val binary_search_left :
  ('a -> 'a -> int) -> 'a array -> int -> int -> 'a -> int
(** search for value [v] in array [a], between indices [fromi] inclusive and
    [toi] exclusive, using comparison function [cmp]. If [v] occurs in [a],
    returns an index immediately to the left of the set of occurrences of [v].
    Otherwise, returns an index where to insert [v]. *)
(*@ r = binary_search_left cmp a fromi toi v
      requires Order.is_pre_order cmp
      requires 0 <= fromi <= toi <= Array.length a
      requires forall i j. fromi <= i <= j < toi -> cmp a.(i) a.(j) <= 0
      ensures  fromi <= r <= toi
      ensures  forall i. fromi <= i < r   -> cmp a.(i) v <  0
      ensures  forall i. r     <= i < toi -> cmp a.(i) v >= 0 *)

val binary_search_right :
  ('a -> 'a -> int) -> 'a array -> int -> int -> 'a -> int
(** search for value [v] in array [a], between indices [fromi] inclusive and
    [toi] exclusive, using comparison function [cmp]. If [v] occurs in [a],
    returns an index immediately to the right of the set of occurrences of [v].
    Otherwise, returns an index where to insert [v]. *)
(*@ r = binary_search_right cmp a fromi toi v
      requires Order.is_pre_order cmp
      checks   0 <= fromi <= toi <= Array.length a
      requires forall i j. fromi <= i <= j < toi -> cmp a.(i) a.(j) <= 0
      ensures  fromi <= r <= toi
      ensures  forall i. fromi <= i < r   -> cmp a.(i) v <= 0
      ensures  forall i. r     <= i < toi -> cmp a.(i) v >  0 *)

val binary_sort : ('a -> 'a -> int) -> 'a array -> int -> int -> unit
(** sort array [a] betweens indices [fromi] inclusive and [toi] exclusive using
    a binary insertion sort. Time complexity is quadratic, but number of
    comparisons is only linearithmic. *)
(*@ binary_sort cmp a fromi toi
      requires Order.is_pre_order cmp
      requires 0 <= fromi <= toi <= Array.length a
      modifies a
      ensures  forall i j. fromi <= i <= j < toi -> cmp a.(i) a.(j) <= 0
      ensures  Array.permut_sub (old a) a fromi toi *)

val swap : 'a array -> int -> int -> unit
(*@ swap a i j
      requires 0 <= i < Array.length a && 0 <= j < Array.length a
      modifies a
      ensures  a.(i) = old a.(j)
      ensures  a.(j) = old a.(i)
      ensures  forall k. 0 <= k < Array.length a ->
               k <> i -> k <> j -> a.(k) = old a.(k) *)

val knuth_shuffle : 'a array -> unit
(*@ knuth_shuffle a
      modifies a
      ensures  Array.permut (old a) a *)
