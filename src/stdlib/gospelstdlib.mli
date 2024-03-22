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
(** The type for finite unordered multisets. *)

(*@ type 'a ref *)
(** The type for references. *)

(*@ type 'a set *)
(** The type for finite unordered sets. *)

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

(** {2 Bitwise operations} *)

(*@ function logand (x y: integer) : integer *)
(*@ function logor (x y: integer) : integer *)
(*@ function logxor (x y: integer) : integer *)
(*@ function lognot (x: integer) : integer *)

(*@ function shift_left (x y: integer) : integer *)
(** Shifts to the left, equivalent to a multiplication by a power of two *)

(*@ function shift_right (x y: integer) : integer *)
(** Shifts to the right, equivalent to a multiplication by a power of two with
    rounding toward -oo *)

(*@ function shift_right_trunc (x y: integer) : integer *)
(** Shift to the right with truncation, equivalent to a multiplication by a
    power of two with rounding toward 0 *)

(** {2 Machine integers}

    There is a coercion from type [int] to type [integer], so that Gospel
    specifications can be written using type [integer] only, and yet use OCaml's
    variables of type [int]. The Gospel typechecker will automatically apply
    [integer_of_int] whenever necessary. *)

(*@ function integer_of_int (x: int) : integer *)
(*@ coercion *)

(*@ function max_int : integer *)
(*@ function min_int : integer *)

(** {1 Couples} *)

(*@ function fst (p: 'a * 'b) : 'a *)
(** [fst (x, y)] is [x]. *)

(*@ function snd (p: 'a * 'b) : 'b *)
(** [snd (x, y)] is [y]. *)

(** {1 References} *)

(*@ function (!_) (r: 'a ref) : 'a *)
(** Reference content access operator. *)

(** {1 Sequences} *)

(*@ function (++) (s s': 'a sequence) : 'a sequence *)
(** [s ++ s'] is the sequence [s] followed by the sequence [s']. *)

(*@ function ([_]) (s: 'a sequence) (i: integer): 'a *)
(** [s[i]] is the [i]th element of the sequence [s]. *)

(*@ function ([_.._]) (s: 'a sequence) (i1: integer) (i2: integer): 'a sequence *)
(*@ function ([_..]) (s: 'a sequence) (i: integer): 'a sequence *)
(*@ function ([.._]) (s: 'a sequence) (i: integer): 'a sequence *)

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

  (*@ function rec fold_left (f: 'a -> 'b -> 'a) (acc: 'a) (s: 'b sequence) : 'a *)
  (** [fold_left f acc s] is [f (... (f (f acc s[0]) s[1]) ...) s[n-1]], where
      [n] is the length of [s]. *)

  (*@ function rec fold_right (f: 'a -> 'b -> 'b) (s: 'a t) (acc: 'b) : 'b *)
  (** [fold_right f s acc] is [f s[1] (f s[2] (... (f s[n] acc) ...))] where [n]
      is the length of [s]. *)
end

(** Lists

    The type ['a list] and the constructors [[]] and [(::)] are built-in. *)

module List : sig
  (*@ type 'a t = 'a list *)
  (** An alias for ['a list]. *)

  (*@ function length (l: 'a t) : integer *)
  (** [length l] is the number of elements of [l]. *)

  (*@ function hd (l: 'a t) : 'a *)
  (** When [l] contains one or more elements, [hd s] is the first element of
      [l]. *)

  (*@ function tl (l: 'a t) : 'a t *)
  (** When [l] contains one or more elements, [tl l] is the list of the elements
      of [l], starting at position 2. *)

  (*@ function nth (l: 'a t) (i: integer) : 'a *)
  (** [nth l i] is the [i]th element of [l]. *)

  (*@ function nth_opt (l: 'a t) (i: integer) : 'a option *)
  (** [nth l i] is the [i]th element of [l] if [i] is within the bounds of [l],
      and [None] otherwise. *)

  (*@ function rev (l: 'a t) : 'a t *)
  (** [rev l] contains the same elements as [l] in a reverse order. *)

  (*@ function init (n: integer) (f: integer -> 'a) : 'a t *)
  (** [init n f] is a list of length [n], with element number [i] initialized
      with [f i]. *)

  (*@ function map (f: 'a -> 'b) (l: 'a t) : 'b t *)
  (** [map f l] applies function [f] to all the elements of [l], and builds a
      list with the results returned by [f] *)

  (*@ function mapi (f: integer -> 'a -> 'b) (l: 'a t) : 'b t *)
  (** Same as {!map}, but the function is applied to the index of the element as
      first argument, and the element itself as second argument. *)

  (*@ function fold_left (f: 'a -> 'b -> 'a) (init: 'a) (l: 'b t) : 'a *)
  (** [fold_left f init t] is [f (... (f (f init a[0]) a[1]) ...) a[n-1]], where
      [n] is the length of [t]. *)

  (*@ function fold_right (f: 'b -> 'a -> 'a) (l: 'b t) (init: 'a) : 'a *)
  (** [fold_right f t init] is [f a[0] (f a[1] ( ... (f a[n-1] init) ...))],
      where [n] is the length of [t]. *)

  (*@ function map2 (f: 'a -> 'b -> 'c) (l: 'a t) (l': 'b t) : 'c t *)
  (** [map2 f l l'] applies function [f] to all the elements of [l] and [l'],
      and builds a list with the results returned by [f]. *)

  (*@ predicate for_all (f: 'a -> bool) (l: 'a t) *)
  (** [for_all f l] holds iff all elements of [l] satisfy the predicate [f]. *)

  (*@ predicate _exists (f: 'a -> bool) (l: 'a t) *)
  (** [_exists f l] holds iff at least one element of [l] satisfies [f]. *)

  (*@ predicate for_all2 (f: 'a -> 'b -> bool) (l: 'a t) (l': 'b t) *)
  (** Same as {!for_all}, but for a two-argument predicate. *)

  (*@ predicate _exists2 (f: 'a -> 'b -> bool) (l: 'a t) (l': 'b t) *)
  (** Same as {!_exists}, but for a two-argument predicate. *)

  (*@ predicate mem (x: 'a) (l: 'a t) *)
  (** [mem x l] holds iff [x] is equal to an element of [l] *)

  (*@ function to_seq (s: 'a t) : 'a Sequence.t *)
  (*@ coercion *)

  (*@ function of_seq (s: 'a Sequence.t) : 'a t *)
end

(** {1 Arrays} *)

module Array : sig
  (*@ type 'a t = 'a array *)
  (** An alias for the type of arrays. *)

  (*@ function length (a: 'a t) : integer *)
  (** [length a] is the number of elements of [a]. *)

  (*@ function get (a: 'a t) (i: integer) : 'a *)
  (** [get a i] is the element number [i] of array [a]. *)

  (*@ function make (n: integer) (x: 'a) : 'a t *)
  (** [make n x] is an array of length [n], initialized with [x]. *)

  (*@ function init (n: integer) (f: integer -> 'a) : 'a t *)
  (** [init n f] is an array of length [n], with element number [i] initialized
      to the result of [f i]. *)

  (*@ function append (a b: 'a t) : 'a t *)
  (** [append v1 v2] returns an array containing the concatenation of [v1] and
      [v2]. *)

  (*@ function concat (a: 'a t list) : 'a t *)
  (** Same as {!append}, but concatenates a list of arrays. *)

  (*@ function sub (a: 'a t) (i len: integer) : 'a t *)
  (** [sub a pos len] is the array of length [len], containing the elements
      number [pos] to [pos + len - 1] of array [a]. *)

  (*@ function map (f: 'a -> 'b) (a: 'a t) : 'b t *)
  (** [map f a] applies function [f] to all the elements of [a], and builds an
      array with the results returned by [f] *)

  (*@ function mapi (f: integer -> 'a -> 'b) (a: 'a t) : 'b t *)
  (** Same as {!map}, but the function is applied to the index of the element as
      first argument, and the element itself as second argument. *)

  (*@ function fold_left (f: 'a -> 'b -> 'a) (init: 'a) (a: 'b t) : 'a *)
  (** [fold_left f init a] is [f (... (f (f init a[0]) a[1]) ...) a[n-1]], where
      [n] is the length of [a]. *)

  (*@ function fold_right (f: 'b -> 'a -> 'a) (a: 'b t) (init: 'a) : 'a *)
  (** [fold_right f a init] is [f a[0] (f a[1] ( ... (f a[n-1] init) ...))],
      where [n] is the length of [a]. *)

  (*@ function map2 (f: 'a -> 'b -> 'c) (a: 'a t) (b: 'b t) : 'c t *)
  (** [map2 f a b] applies function [f] to all the elements of [a] and [b], and
      builds an array with the results returned by [f]. *)

  (*@ predicate for_all (f: 'a -> bool) (a: 'a t) *)
  (** [for_all f a] holds iff all elements of [a] satisfy the predicate [f]. *)

  (*@ predicate _exists (f: 'a -> bool) (a: 'a t) *)
  (** [_exists f a] holds iff at least one element of [a] satisfies [f]. *)

  (*@ predicate for_all2 (f: 'a -> 'b -> bool) (a: 'a t) (b: 'b t) *)
  (** Same as {!for_all}, but for a two-argument predicate. *)

  (*@ predicate _exists2 (f: 'a -> 'b -> bool) (a: 'a t) (b: 'b t) *)
  (** Same as {!_exists}, but for a two-argument predicate. *)

  (*@ predicate mem (x: 'a) (a: 'a t) *)
  (** [mem x a] holds iff [x] is equal to an element of [a] *)

  (*@ function to_list (a: 'a t) : 'a list *)
  (*@ function of_list (l: 'a list) : 'a t *)

  (*@ function to_seq (a: 'a t) : 'a Sequence.t *)
  (*@ coercion *)
  (*@ function of_seq (s: 'a Sequence.t) : 'a t *)

  (*@ function to_bag (a: 'a t) : 'a bag *)

  (*@ predicate permut (a b: 'a array) *)
  (** [permut a b] is true iff [a] and [b] contain the same elements with the
      same number of occurrences *)

  (*@ predicate permut_sub (a b: 'a array) (lo hi: integer) *)
  (** [permut_sub a b lo hi] is true iff the segment `a1.(lo..hi-1)` is a
      permutation of the segment `a2.(lo..hi-1)` and values outside of the
      interval are equal. *)
end

(** {1 Bags} *)

module Bag : sig
  (*@ type 'a t = 'a bag *)
  (** An alias for ['a bag]. *)

  (*@ function occurrences (x: 'a) (b: 'a t): integer *)
  (** [occurrences x b] is the number of occurrences of [x] in [s]. *)

  (*@ function empty : 'a t *)
  (** [empty] is the empty bag. *)

  (*@ predicate is_empty (b: 'a t) *)
  (** [is_empty b] is [b = empty]. *)

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

  (*@ function choose (b: 'a t) : 'a *)
  (** [choose b] is an arbitrary element of [b]. *)

  (*@ function choose_opt (b: 'a t) : 'a option *)
  (** [choose_opt b] is an arbitrary element of [b] or [None] if [b] is empty. *)

  (*@ function map (f: 'a -> 'b) (b: 'a t) : 'b t *)
  (** [map f b] is a fresh bag which elements are [f x1 ... f xN], where
      [x1 ... xN] are the elements of [b]. *)

  (*@ function fold (f: 'a -> 'b -> 'b) (b: 'a t) (a: 'b) : 'b *)
  (** [fold f b a] is [(f xN ... (f x2 (f x1 a))...)], where [x1 ... xN] are the
      elements of [b]. *)

  (*@ predicate for_all (f: 'a -> bool) (b: 'a t) *)
  (** [for_all f b] holds iff [f x] is [true] for all elements in [b]. *)

  (*@ predicate _exists (f: 'a -> bool) (b: 'a t) *)
  (** [for_all f b] holds iff [f x] is [true] for at least one element in [b]. *)

  (*@ function filter (f: 'a -> bool) (b: 'a t) : 'a t *)
  (** [filter f b] is the bag of all elements in [b] that satisfy [f]. *)

  (*@ function filter_map (f: 'a -> 'a option) (b: 'a t) : 'a t *)
  (** [filter_map f b] is the bag of all [v] such that [f x = Some v] for some
      element [x] of [b]. *)

  (*@ function partition (f: 'a -> bool) (b: 'a t) : ('a t * 'a t) *)
  (** [partition f b] is the pair of bags [(b1, b2)], where [b1] is the bag of
      all the elements of [b] that satisfy [f], and [b2] is the bag of all the
      elements of [b] that do not satisfy [f]. *)

  (*@ function cardinal (b: 'a t) : integer *)
  (** [cardinal b] is the total number of elements in [b], all occurrences being
      counted. *)

  (*@ function to_list (b: 'a t) : 'a list *)
  (*@ function of_list (l: 'a list) : 'a t *)

  (*@ function to_seq (b: 'a t) : 'a Sequence.t *)
  (*@ function of_seq (s: 'a Sequence.t) : 'a t *)
end

(** {1 Sets} *)

(*@ function ({}) : 'a set *)
(** [{}] is the empty set. *)

module Set : sig
  (*@ type 'a t = 'a set *)
  (** An alias for ['a set]. *)

  (*@ function compare (s s': 'a t) : integer *)
  (** A comparison function over sets. *)

  (*@ function empty : 'a t *)
  (** [empty] is [∅]. *)

  (*@ predicate is_empty (s: 'a t) *)
  (** [is_empty s] is [s = ∅]. *)

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

  (*@ function choose (s: 'a t) : integer *)
  (** [choose s] is an arbitrary element of [s]. *)

  (*@ function choose_opt: 'a t -> 'a option *)
  (** [choose_opt s] is an arbitrary element of [s] or [None] if [s] is empty. *)

  (*@ function map (f: 'a -> 'b) (s: 'a t) : 'b t *)
  (** [map f s] is a fresh set which elements are [f x1 ... f xN], where
      [x1 ... xN] are the elements of [s]. *)

  (*@ function fold (f: 'a -> 'b -> 'b) (s: 'a t) (a: 'b) : 'b *)
  (** [fold f s a] is [(f xN ... (f x2 (f x1 a))...)], where [x1 ... xN] are the
      elements of [s]. *)

  (*@ predicate for_all (f: 'a -> bool) (s: 'a t) *)
  (** [for_all f s] holds iff [f x] is [true] for all elements in [s]. *)

  (*@ predicate _exists (f: 'a -> bool) (s: 'a t) *)
  (** [_exists f s] holds iff [f x] is [true] for at least one element in [s]. *)

  (*@ function filter (f: 'a -> bool) (s: 'a t) : 'a t *)
  (** [filter f s] is the set of all elements in [s] that satisfy [f]. *)

  (*@ function filter_map (f: 'a -> 'a option) (s: 'a t) : 'a t *)
  (** [filter_map f s] is the set of all [v] such that [f x = Some v] for some
      element [x] of [s]. *)

  (*@ function partition (f: 'a -> bool) (s: 'a t) : ('a t * 'a t) *)
  (** [partition f s] is the pair of sets [(s1, s2)], where [s1] is the set of
      all the elements of [s] that satisfy the predicate [f], and [s2] is the
      set of all the elements of [s] that do not satisfy [f]. *)

  (*@ function to_list (s: 'a t) : 'a list *)
  (*@ function of_list (l: 'a list) : 'a t *)

  (*@ function to_seq (s: 'a t) : 'a Sequence.t *)
  (*@ function of_seq (s: 'a Sequence.t) : 'a t *)
end

(*@ function ( [->] ) (f: 'a -> 'b) (x:'a) (y: 'b) : 'a -> 'b *)

module Map : sig
  (** Maps from keys of type ['a] to values of type ['b] are represented by
      Gospel functions of type ['a -> 'b]. *)
end

module Order : sig
  (*@ predicate is_pre_order (cmp: 'a -> 'a -> int) =
      (forall x. cmp x x = 0) /\
      (forall x y. cmp x y <= 0 <-> cmp y x >= 0) /\
      (forall x y z.
         (cmp x y <= 0 -> cmp y z <= 0 -> cmp x z <= 0) /\
         (cmp x y <= 0 -> cmp y z <  0 -> cmp x z <  0) /\
         (cmp x y <  0 -> cmp y z <= 0 -> cmp x z <  0) /\
         (cmp x y <  0 -> cmp y z <  0 -> cmp x z <  0)) *)
end

(** Other OCaml built-in stuff *)

exception Not_found
exception Invalid_argument of string
exception Failure of string

module Sys : sig
  (*@ function word_size : integer *)

  (*@ function int_size : integer *)

  (*@ function big_endian : bool *)

  (*@ function max_string_length : integer *)

  (*@ function max_array_length : integer *)
end
