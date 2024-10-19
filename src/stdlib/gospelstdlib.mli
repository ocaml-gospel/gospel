(**************************************************************************)
(*                                                                        *)
(*  Gospel -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** This file contains the Gospel standard library. *)

(** The following are not defined in the Gospelstdlib but are built-in in
    Gospel:

    - [type unit]
    - [type string]
    - [type char]
    - [type float]
    - [type bool]
    - [type integer]
    - [type int]

    - [type 'a option]
    - [function None: 'a option]
    - [function Some (x: 'a) : 'a option]

    - [type 'a list]
    - [function ([]): 'a list]
    - [function (::) (x: 'a) (l: 'a list) : 'a list]

    - [predicate (=) (x y: 'a)] *)

(** The rest of this module is the actual content of the Gospel module
    [Gospelstdlib]. This module is automatically opened in Gospel
    specifications. *)

(*@ type 'a sequence *)
(** The type for finite sequences. *)

(*@ type 'a bag *)
(** The type for multisets. *)

(*@ type 'a set *)
(** The type for sets. *)

(*@ type ('a, 'b) map = 'a -> 'b*)
(** The type for total maps *)

(** {1 Arithmetic}

    The type [integer] is built-in. This is the type of arbitrary precision
    integers, not to be confused with OCaml's type [int] (machine, bounded
    integers). *)

(*@ function succ (x: integer) : integer *)
(*@ function pred (x: integer) : integer *)

(*@ function (-_) (x: integer) : integer *)
(*@ function (+) (x y: integer) : integer *)
(*@ function (-) (x y: integer) : integer *)
(*@ function ( * ) (x y: integer) : integer *)
(*@ function (/) (x y: integer) : integer *)
(*@ function mod (x y: integer) : integer *)

(*@ function pow (x y: integer) : integer *)
(*@ function abs (x:integer) : integer *)

(*@ function min (x y : integer) : integer *)
(*@ function max (x y : integer) : integer *)

(** {2 Comparisons} *)

(*@ predicate (>) (x y: integer) *)
(*@ predicate (>=) (x y: integer) *)
(*@ predicate (<) (x y: integer) *)
(*@ predicate (<=) (x y: integer) *)

(*@ function integer_of_int (x: int) : integer *)
(*@ coercion *)

(*@ function max_int : integer *)
(*@ function min_int : integer *)

(** {1 Sequences} *)

(*@ function (++) (s s': 'a sequence) : 'a sequence *)
(** [s ++ s'] is the sequence [s] followed by the sequence [s']. *)

(*@ function ([_]) (s: 'a sequence) (i: integer): 'a *)
(** [s[i]] is the [i]th element of the sequence [s]. *)

(*@ function ([_.._]) (s: 'a sequence) (i1: integer) (i2: integer): 'a sequence *)
(*@ function ([_..]) (s: 'a sequence) (i: integer): 'a sequence *)
(*@ function ([.._]) (s: 'a sequence) (i: integer): 'a sequence *)

(*@ predicate monoid (f : 'a -> 'a -> 'a) (neutral : 'a) *)

(*@ axiom monoid_def :
      forall f neutral.
      monoid f neutral <->
        (forall x. (f neutral x = f x neutral = x)) /\
        (forall x y z. f x (f y z) = f (f x y) z) *)

(*@ predicate comm_monoid (f : 'a -> 'a -> 'a) (neutral : 'a) *)

(*@ axiom comm_monoid_def :
      forall f neutral.
      comm_monoid f neutral <->
        monoid f neutral /\
        (forall x y. f x y = f y x) *)

module Sequence : sig
  (*@ type 'a t = 'a sequence *)
  (** An alias for {!sequence} *)

  (*@ function length (s: 'a t): integer *)
  (** [length s] is the length of the sequence [s]. *)

  (*@ function empty : 'a t *)
  (** [empty] is the empty sequence. *)

  (*@ function singleton (x: 'a) : 'a t *)
  (** [singleton] is an alias for {!return}. *)

  (*@ function init (n: integer) (f: integer -> 'a) : 'a t *)
  (** [init n f] is the sequence containing [f 0], [f 1], [...] , [f n]. *)

  (*@ function cons (x: 'a) (s: 'a t): 'a t *)
  (** [cons x s] is the sequence containing [x] followed by the elements of [s]. *)

  (*@ function snoc (s: 'a t) (x: 'a): 'a t *)
  (** [snoc s x] is the sequence containing the elements of [s] followed by [x]. *)

  (*@ function hd (s: 'a t) : 'a *)
  (** When [s] contains one or more elements, [hd s] is the first element of
      [s]. *)

  (*@ function tl (s: 'a t) : 'a t *)
  (** When [s] contains one or more elements, [tl s] is the sequence of the
      elements of [s], starting at position 2. *)

  (*@ function append (s s': 'a t) : 'a t *)
  (** [append s s'] is [s ++ s']. *)

  (*@ predicate mem (s: 'a t) (x: 'a) *)
  (** [mem s x] holds iff [x] is in [s]. *)

  (*@ function map (f: 'a -> 'b) (s: 'a t) : 'b t *)
  (** [map f s] is a sequence whose elements are the elements of [s],
      transformed by [f]. *)

  (*@ function filter (f: 'a -> bool) (s: 'a t) : 'a t *)
  (** [filter f s] is a sequence whose elements are the elements of [s], that
      satisfy [f]. *)

  (*@ function filter_map (f: 'a -> 'b option) (s: 'a t) : 'b t *)
  (** [filter_map f s] is a sequence whose elements are the elements of [s],
      transformed by [f]. An element [x] is dropped whenever [f x] is [None]. *)

  (*@ function get (s: 'a t) (i: integer) : 'a *)
  (** [get s i] is [s[i]]. *)

  (*@ function set (s: 'a t) (i: integer) (x: 'a): 'a t *)
  (** [set s i x] is the sequence [s] where the [i]th element is [x]. *)

  (*@ function rev (s: 'a t) : 'a t *)
  (** [rev s] is the sequence containing the same elements as [s], in reverse
      order. *)
  (* <---- to be removed *)
  (*@ function rec fold_left (f: 'a -> 'b -> 'a) (acc: 'a) (s: 'b sequence) : 'a *)

  (*@ function rec fold_right (f: 'a -> 'b -> 'b) (s: 'a t) (acc: 'b) : 'b *)
end

(** {1 Bags} *)

module Bag : sig
  (*@ type 'a t = 'a bag *)
  (** An alias for ['a bag]. *)

  (*@ function multiplicity (x: 'a) (b: 'a t): integer *)
  (** [occurrences x b] is the number of occurrences of [x] in [s]. *)

  (*@ function empty : 'a t *)
  (** [empty] is the empty bag. *)

  (*@ predicate mem (x: 'a) (b: 'a t) *)
  (** [mem x b] holds iff [b] contains [x] at least once. *)

  (*@ function add (x: 'a) (b: 'a t) : 'a t *)
  (** [add x b] is [b] when an occurence of [x] was added. *)

  (*@ function singleton (x: 'a) : 'a t *)
  (** [singleton x] is a bag containing one occurence of [x]. *)

  (*@ function remove (x: 'a) (b: 'a t) : 'a t *)
  (** [remove x b] is [b] where an occurence of [x] was removed. *)

  (*@ function union (b b': 'a t) : 'a t *)
  (** [union b b'] is a bag [br] where for all element [x],
      [occurences x br = max
      (occurences x b) (occurences x b')]. *)

  (*@ function sum (b b': 'a t) : 'a t *)
  (** [sum b b'] is a bag [br] where for all element [x],
      [occurences x br =
      (occurences x b) + (occurences x b')]. *)

  (*@ function inter (b b': 'a t) : 'a t *)
  (** [inter b b'] is a bag [br] where for all element [x],
      [occurences x br =
      min (occurences x b) (occurences x b')]. *)

  (*@ predicate disjoint (b b': 'a t) *)
  (** [disjoint b b'] holds iff [b] and [b'] have no element in common. *)

  (*@ function diff (b b': 'a t) : 'a t *)
  (** [diff b b'] is a bag [br] where for all element [x],
      [occurences x br =
      max 0 (occurences x b - occurences x b')]. *)

  (*@ predicate subset (b b': 'a t) *)
  (** [subset b b'] holds iff for all element [x],
      [occurences x b <= occurences x b']. *)

  (*@ function filter (f: 'a -> bool) (b: 'a t) : 'a t *)
  (** [filter f b] is the bag of all elements in [b] that satisfy [f]. *)

  (*@ function cardinal (b: 'a t) : integer *)
  (** [cardinal b] is the total number of elements in [b], all occurrences being
      counted. *)

  (*@ function of_list (l: 'a list) : 'a sequence *)
  (*@ coercion *)
end

(** {1 Sets} *)

(*@ function ({}) : 'a set *)
(** [{}] is the empty set. *)

module Set : sig
  (*@ type 'a t = 'a set *)
  (** An alias for ['a set]. *)

  (*@ function empty : 'a t *)
  (** [empty] is [∅]. *)

  (*@ predicate mem (x: 'a) (s: 'a t) *)
  (** [mem x s] is [x ∈ s]. *)

  (*@ function add (x: 'a) (s: 'a t) : 'a t *)
  (** [add x s] is [s ∪ {x}]. *)

  (*@ function singleton (x: 'a) : 'a t *)
  (** [singleton x] is [{x}]. *)

  (*@ function remove (x: 'a) (s: 'a t) : 'a t *)
  (** [remove x s] is [s ∖ {x}]. *)

  (*@ function union (s s': 'a t) : 'a t *)
  (** [union s s'] is [s ∪ s']. *)

  (*@ function inter (s s': 'a t) : 'a t *)
  (** [inter s s'] is [s ∩ s']. *)

  (*@ predicate disjoint (s s': 'a t) *)
  (** [disjoint s s'] is [s ∩ s' = ∅]. *)

  (*@ function diff (s s': 'a t) : 'a t *)
  (** [diff s s'] is [s ∖ s']. *)

  (*@ predicate subset (s s': 'a t) *)
  (** [subset s s'] is [s ⊂ s']. *)

  (*@ function cardinal (s: 'a t) : integer *)
  (** [cardinal s] is the number of elements in [s]. *)

  (*@ function map (f: 'a -> 'b) (s: 'a t) : 'b t *)
  (** [map f s] is a fresh set which elements are [f x1 ... f xN], where
      [x1 ... xN] are the elements of [s]. *)

  (*@ function fold (f: 'a -> 'b -> 'b) (s: 'a t) (a: 'b) : 'b *)
  (** [fold f s a] is [(f xN ... (f x2 (f x1 a))...)], where [x1 ... xN] are the
      elements of [s]. *)

  (*@ function partition (f: 'a -> bool) (s: 'a t) : ('a t * 'a t) *)
  (** [partition f s] is the pair of sets [(s1, s2)], where [s1] is the set of
      all the elements of [s] that satisfy the predicate [f], and [s2] is the
      set of all the elements of [s] that do not satisfy [f]. *)

  (*@ function of_seq (s: 'a Sequence.t) : 'a t *)
end

(*@ function ( [->] ) (f: 'a -> 'b) (x:'a) (y: 'b) : 'a -> 'b *)

(*@ axiom map_set_def :
    forall f : ('a -> 'b).
    forall x : 'a.
    forall y : 'b.
    f[x -> y] = fun arg -> if arg = x then y else f x *)

module Map : sig
  (** Maps from keys of type ['a] to values of type ['b] are represented by
      Gospel functions of type ['a -> 'b]. *)
end

(* The following modules are deprecated and only exist to ensure the tests pass. In the future, assuming this branch is merged, they should be removed and the tests changed*)

module Array : sig
  (*@ function get (a : 'a array) (i : integer) : 'a *)

  (*@ function length (a : 'a array) : integer *)
  (*@ function to_seq (a : 'a array) : 'a sequence *)
  (*@ coercion *)
  (*@ predicate permut (a1 : 'a array) (b1 : 'a array) *)
  (*@ predicate permut_sub (a1 : 'a array) (a2 : 'a array) (i : integer) (j : integer) *)

end

module List : sig
  (*@ function fold_left (f : 'b -> 'a -> 'b) (acc : 'b) (l : 'a list) : 'b *)
  (*@ predicate _exists (f : 'a -> bool) (l : 'a list) *)
  (*@ function length (l : 'a list) : integer *)
  (*@ function nth (l : 'a list) (i : integer) : 'a *)
  (*@ predicate mem (x : 'a) (l : 'a list) *)
  (*@ function map (f : 'a -> 'b) (l : 'a list) : 'b list *)
end

module Order : sig
  (*@ predicate is_pre_order (f: 'a -> 'a -> int) *)
end
(*@ type 'a ref *)
(*@ function (!_) (r : 'a ref) : 'a *)
(*@ function logand (n1 : integer) (n2: integer) : integer *)
