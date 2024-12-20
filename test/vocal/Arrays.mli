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
      requires Order.is_pre_order (fun x y -> (cmp x y).v)
      requires 0 <= fromi.v <= toi.v <= Sequence.length a
      requires forall i j. fromi.v <= i <= j < toi.v -> (cmp a[i] a[j]).v <= 0
      ensures  fromi.v <= r.v < toi.v && (cmp a[r.v] v).v = 0
      raises   Not_found -> forall i. fromi.v <= i < toi.v -> (cmp a[i] v).v <> 0 *)

val binary_search_left :
  ('a -> 'a -> int) -> 'a array -> int -> int -> 'a -> int
(** search for value [v] in array [a], between indices [fromi] inclusive and
    [toi] exclusive, using comparison function [cmp]. If [v] occurs in [a],
    returns an index immediately to the left of the set of occurrences of [v].
    Otherwise, returns an index where to insert [v]. *)
(*@ r = binary_search_left cmp a fromi toi v
      requires Order.is_pre_order (fun x y -> (cmp x y).v)
      requires 0 <= fromi.v <= toi.v <= Sequence.length a
      requires forall i j. fromi.v <= i <= j < toi.v -> (cmp a[i] a[j]).v <= 0
      ensures  fromi.v <= r.v <= toi.v
      ensures  forall i. fromi.v <= i < r.v   -> (cmp a[i] v).v <  0
      ensures  forall i. r.v     <= i < toi.v -> (cmp a[i] v).v >= 0 *)

val binary_search_right :
  ('a -> 'a -> int) -> 'a array -> int -> int -> 'a -> int
(** search for value [v] in array [a], between indices [fromi] inclusive and
    [toi] exclusive, using comparison function [cmp]. If [v] occurs in [a],
    returns an index immediately to the right of the set of occurrences of [v].
    Otherwise, returns an index where to insert [v]. *)
(*@ r = binary_search_right cmp a fromi toi v
      requires Order.is_pre_order (fun x y -> (cmp x y).v)
      checks   0 <= fromi.v <= toi.v <= Sequence.length a
      requires forall i j. fromi.v <= i <= j < toi.v -> (cmp a[i] a[j]).v <= 0
      ensures  fromi.v <= r.v <= toi.v
      ensures  forall i. fromi.v <= i < r.v   -> (cmp a[i] v).v <= 0
      ensures  forall i. r.v     <= i < toi.v -> (cmp a[i] v).v >  0 *)

val binary_sort : ('a -> 'a -> int) -> 'a array -> int -> int -> unit
(** sort array [a] betweens indices [fromi] inclusive and [toi] exclusive using
    a binary insertion sort. Time complexity is quadratic, but number of
    comparisons is only linearithmic. *)
(*@ binary_sort cmp a fromi toi
      requires Order.is_pre_order (fun x y -> (cmp x y).v)
      requires 0 <= fromi.v <= toi.v <= Sequence.length a
      modifies a
      ensures  forall i j. fromi.v <= i <= j < toi.v -> (cmp a[i] a[j]).v <= 0
      ensures  Sequence.permut_sub (old a) a fromi.v toi.v *)

val swap : 'a array -> int -> int -> unit
(*@ swap a i j
      requires 0 <= i.v < Sequence.length a && 0 <= j.v < Sequence.length a
      modifies a
      ensures  a[i.v] = old a[j.v]
      ensures  a[j.v] = old a[i.v]
      ensures  forall k. 0 <= k < Sequence.length a ->
               k <> i.v -> k <> j.v -> a[k] = old a[k] *)

val knuth_shuffle : 'a array -> unit
(*@ knuth_shuffle a
      modifies a
      ensures  Sequence.permut (old a) a *)
