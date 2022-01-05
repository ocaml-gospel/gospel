(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module Make (X : sig
  (* FIXME: use ComparableType.S instead *)
  type t

  (*@ function cmp: t -> t -> int *)

  (*@ axiom is_pre_order: Order.is_pre_order cmp *)

  val compare : t -> t -> int
  (*@ r = compare x y
        ensures r = cmp x y *)
end) : sig
  type elt = X.t
  type t
  (*@ model bag : elt bag *)

  val empty : unit -> t
  (*@ h = empty ()
        ensures Bag.cardinal h.bag = 0
        ensures forall x. Bag.occurrences x h.bag = 0 *)

  val is_empty : t -> bool
  (*@ b = is_empty h
        ensures b <-> Bag.is_empty h.bag *)

  val merge : t -> t -> t
  (*@ h = merge h1 h2
        ensures Bag.cardinal h.bag = Bag.cardinal h1.bag + Bag.cardinal h2.bag
        ensures forall x. Bag.occurrences x h.bag = Bag.occurrences x h1.bag + Bag.occurrences x h2.bag *)

  val insert : elt -> t -> t
  (*@ h' = insert x h
        ensures Bag.occurrences x h'.bag = Bag.occurrences x h.bag + 1
        ensures forall y. y <> x -> Bag.occurrences y h'.bag = Bag.occurrences y h.bag
        ensures Bag.cardinal h'.bag = Bag.cardinal h.bag + 1 *)

  (*@ predicate mem        (x: elt) (h: t) = Bag.occurrences x h.bag > 0 *)
  (*@ predicate is_minimum (x: elt) (h: t) =
        mem x h /\ forall e. mem e h -> X.cmp x e <= 0 *)

  (*@ function minimum (h: t) : elt *)
  (*@ axiom min_def: forall h. 0 < Bag.cardinal h.bag -> is_minimum (minimum h) h *)

  val find_min : t -> elt
  (*@ x = find_min h
        requires Bag.cardinal h.bag > 0
        ensures  x = minimum h *)

  val delete_min : t -> t
  (*@ h' = delete_min h
        requires Bag.cardinal h.bag > 0
        ensures  let x = minimum h in Bag.occurrences x h'.bag = Bag.occurrences x h.bag - 1
        ensures  forall y. y <> minimum h -> Bag.occurrences y h'.bag = Bag.occurrences y h.bag
        ensures  Bag.cardinal h'.bag = Bag.cardinal h.bag - 1 *)
end
