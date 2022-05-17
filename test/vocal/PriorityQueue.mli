(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** This module implements a priority queue based on a minimal binary heap. The
    heap is modelized by a dynamic array, taken from the module Vector **)

module Make (X : sig
  type t

  val dummy : t

  (*@ function cmp : t -> t -> int *)

  (*@ axiom is_pre_order: Order.is_pre_order cmp *)

  val compare : t -> t -> int
  (*@ r = compare x y
        ensures r = cmp x y *)
end) : sig
  type elt = X.t
  type heap
  (*@ mutable model bag : elt bag
      with self
      invariant Bag.cardinal self.bag <= Sys.max_array_length *)

  (*@ predicate mem (x: elt) (h: heap) = Bag.occurrences x h.bag > 0 *)

  val create : unit -> heap
  (*@ h = create ()
      ensures h.bag = Bag.empty *)

  val is_empty : heap -> bool
  (*@ b = is_empty h
      ensures b <-> Bag.is_empty h.bag *)

  val size : heap -> int
  (*@ x = size h
      ensures x = Bag.cardinal h.bag *)

  (*@ function minimum (h: heap) : elt *)

  (*@ predicate is_minimum (x: elt) (h: heap) =
        mem x h && forall e. mem e h -> X.cmp x e <= 0 *)

  (*@ axiom min_def:
        forall h. 0 < Bag.cardinal h.bag -> is_minimum (minimum h) h *)

  val find_min : heap -> elt option
  (*@ r = find_min h
      ensures match r with
      | None   -> Bag.cardinal h.bag = 0
      | Some x -> Bag.cardinal h.bag > 0 && x = minimum h *)

  exception Empty

  val find_min_exn : heap -> elt
  (*@ x = find_min_exn h
      raises  Empty -> Bag.cardinal h.bag = 0
      ensures Bag.cardinal h.bag > 0 && x = minimum h *)

  val delete_min_exn : heap -> unit
  (*@ delete_min_exn h
      modifies h
      raises  Empty -> Bag.cardinal h.bag = 0 && h.bag = old h.bag
      ensures (old h).bag = Bag.add (minimum (old h)) h.bag *)

  val extract_min_exn : heap -> elt
  (*@ x = extract_min_exn h
      modifies h
      raises  Empty -> Bag.cardinal h.bag = 0 && h.bag = old h.bag
      ensures x = minimum (old h)
      ensures (old h).bag = Bag.add x h.bag *)

  val insert : elt -> heap -> unit
  (*@ insert x h
      checks   Bag.cardinal h.bag < Sys.max_array_length
      modifies h
      ensures  h.bag = Bag.add x (old h).bag *)
end
