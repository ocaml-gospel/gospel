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

(** Logical symbols *)

(*@ predicate (->) (p : bool) (q : bool) *)

(*@ axiom imp_refl :
      forall p. p -> p *)

(*@ axiom imp_false :
      forall p. false -> p *)

(*@ axiom imp_true :
      forall p. p -> true *)

(*@ predicate (/\) (p : bool) (q : bool) *)

(*@ axiom conj_def :
      forall p q. p -> q -> p /\ q *)

(*@ predicate (\/) (p : bool) (q : bool) *)

(*@ axiom disj_left :
      forall p q. p -> p \/ q *)

(*@ axiom disj_right :
      forall p q. p -> q \/ p *)

(*@ predicate (<->) (p : bool) (q : bool) *)

(*@ axiom iff_def :
      forall p q. (p <-> q) -> (p -> q /\ q -> p)  *)

(*@ function (&&) (b1 : bool) (b2 : bool) : bool *)

(*@ axiom andb_def :
      forall b1 b2.
      b1 && b2 -> b1 = true /\ b2 = true *)

(*@ function (||) (b1 : bool) (b2 : bool) : bool *)

(*@ axiom orb_def :
      forall b1 b2.
      b1 || b2 -> b1 = true \/ b2 = true *)

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

(*@ function to_seq (a : 'a array) : 'a sequence *)
(*@ coercion *)

(*@ function of_list (s : 'a list) : 'a sequence *)
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

  (*@ predicate in_range (s : 'a t) (i : integer) *)

  (*@ axiom in_range_def :
        forall s i.
        in_range s i <-> 0 <= i < length s *)

  (*@ axiom length_nonneg :
        forall s.
        0 <= length s *)

  (*@ axiom subseq_l :
        forall s i.
        in_range s i ->
        s[i ..] = s[i .. length s] *)

  (*@ axiom subseq_r :
        forall s i.
        in_range s i ->
        s[..i] = s[0 .. i] *)

  (*@ axiom subseq :
        forall s i i1 i2.
        0 <= i1 <= i < i2 <= length s ->
        s[i] = (s[i1 .. i2])[i-i1] *)

  (*@ axiom subseq_len :
        forall s i1 i2.
        0 <= i1 <= i2 < length s ->
        length (s[i1 .. i2]) = i2 - i1 *)

  (*@ function empty : 'a t *)
  (** [empty] is the empty sequence. *)

  (*@ axiom empty_length : length empty = 0 *)

  (*@ function init (n: integer) (f: integer -> 'a) : 'a t *)
  (** [init n f] is the sequence containing [f 0], [f 1], [...] , [f (n - 1)].
  *)

  (*@ axiom init_length :
        forall n f.
        n >= 0 ->
        length (init n f) = n *)

  (*@ axiom init_elems :
        forall n f i.
        0 <= i < n -> (init n f)[i] = f i *)

  (*@ function singleton (x: 'a) : 'a t *)
  (** [singleton] is the sequence containing [x]. *)

  (*@ axiom singleton_def :
        forall x f.
        f 0 = x ->
        singleton x = init 1 f *)

  (*@ function cons (x: 'a) (s: 'a t): 'a t *)
  (** [cons x s] is the sequence containing [x] followed by the elements of [s].
  *)

  (*@ axiom cons_def :
        forall x s.
        cons x s = (singleton x) ++ s *)

  (*@ function snoc (s: 'a t) (x: 'a): 'a t *)
  (** [snoc s x] is the sequence containing the elements of [s] followed by [x].
  *)

  (*@ axiom snoc_def :
        forall s x.
        snoc s x = s ++ (singleton x) *)

  (*@ function hd (s: 'a t) : 'a *)
  (** When [s] contains one or more elements, [hd s] is the first element of
      [s]. *)

  (*@ axiom hd_def :
        forall s.
        hd s = s[0] *)

  (*@ function tl (s: 'a t) : 'a t *)
  (** When [s] contains one or more elements, [tl s] is the sequence of the
      elements of [s], starting at position 2. *)

  (*@ axiom tl_def :
        forall s.
        tl s = s[1 ..] *)

  (*@ function append (s1 : 'a t) (s2 : 'a t) : 'a t *)
  (** [append s s'] is [s ++ s']. *)

  (*@ axiom append_def :
        forall s1 s2.
        append s1 s2 = s1 ++ s2 *)

  (*@ axiom append_length :
        forall s s'.
        length (s++s') = length s + length s' *)

  (*@ axiom append_elems_left :
        forall s s' i.
        in_range s i ->
        (s ++ s')[i] = s[i] *)

  (*@ axiom append_elems_right :
        forall s s' i.
        length s <= i < length s + length s' ->
        (s ++ s')[i] = s'[i - length s] *)

  (*@ function multiplicity (x : 'a) (s: 'a t) : integer *)
  (** [multiplicity x s] counts the number of occurrences of [x] in [s] *)

  (*@ axiom mult_empty :
        forall x.
        multiplicity x empty = 0 *)

  (*@ axiom mult_cons :
        forall s x.
        1 + multiplicity x s = multiplicity x (cons x s) *)

  (*@ axiom mult_cons_neutral :
        forall s x1 x2.
        x1 <> x2 ->
        multiplicity x1 s = multiplicity x1 (cons x2 s) *)

  (*@ axiom mult_length :
        forall x s.
        0 <= multiplicity x s <= length s *)

  (*@ predicate mem (x: 'a) (s: 'a t) *)
  (** [mem s x] holds iff [x] is in [s]. *)

  (*@ axiom mem_def :
        forall s x.
        mem x s <-> multiplicity x s > 0 *)

  (*@ predicate _forall (p : 'a -> bool) (s : 'a sequence) *)
  (** [_forall p s] holds iff all elements in [s] satisfy predicate [p] *)

  (*@ axiom forall_def :
        forall p s.
        _forall p s <->
        (forall x. mem x s -> p x)
  *)

  (*@ predicate _exists (p : 'a -> bool) (s : 'a sequence) *)
  (** [_exists p s] holds iff there exists some element in [s] that satisfies
      the predicate [p] *)

  (*@ axiom _exists_def :
        forall p s.
        _exists p s <->
          (exists x. mem x s && p x) *)

  (*@ function map (f: 'a -> 'b) (s: 'a t) : 'b t *)
  (** [map f s] is a sequence whose elements are the elements of [s],
      transformed by [f]. *)

  (*@ axiom map_elems :
        forall i f s.
        in_range s i ->
        (map f s)[i] = f (s[i]) *)

  (*@ function filter (f: 'a -> bool) (s: 'a t) : 'a t *)
  (** [filter f s] is a sequence whose elements are the elements of [s], that
      satisfy [f]. *)

  (*@ axiom filter_elems :
        forall f s x.
        mem x s -> f x ->
        mem x (filter f s) *)

  (*@ function filter_map (f: 'a -> 'b option) (s: 'a t) : 'b t *)
  (** [filter_map f s] is a sequence whose elements are the elements of [s],
      transformed by [f]. An element [x] is dropped whenever [f x] is [None]. *)

  (*@ axiom filter_map_elems :
        forall f s y.
        (exists x. f x = Some y && mem x s) <->
        mem y (filter_map f s) *)

  (*@ function get (s: 'a t) (i: integer) : 'a *)
  (** [get s i] is [s[i]]. *)

  (*@ axiom get_def :
        forall s i.
        get s i = s[i] *)

  (*@ function set (s: 'a t) (i: integer) (x: 'a): 'a t *)
  (** [set s i x] returns a new sequence identical to x where the only
      difference is that the element at index [i] equal [x] *)

  (*@ axiom set_elem :
        forall s i x.
        in_range s i ->
        (set s i x)[i] = x *)

  (*@ axiom set_elem_other :
        forall s i1 i2 x.
        i1 <> i2 ->
        in_range s i1 ->
        in_range s i2 ->
        (set s i1 x)[i2] = s[i2] *)

  (*@ function rev (s: 'a t) : 'a t *)
  (** [rev s] is the sequence containing the same elements as [s], in reverse
      order. *)

  (*@ axiom rev_length :
        forall s.
        length s = length (rev s) *)

  (*@ axiom rev_elems :
        forall i s.
        in_range s i ->
        (rev s)[i] = s[length s - 1 - i] *)

  (*@ axiom extensionality :
        forall s1 s2.
        length s1 = length s2 ->
        (forall i. in_range s1 i -> s1[i] = s2[i]) ->
        s1 = s2 *)

  (*@ function fold_left (f: 'a -> 'b -> 'a) (acc: 'a) (s: 'b t) : 'a *)
  (** [fold_left f acc s] is [f (... (f (f acc s[0]) s[1]) ...) s[n-1]], where
      [n] is the length of [s]. *)

  (*@ axiom fold_left_empty :
        forall f acc.
        fold_left f acc empty = acc *)

  (*@ axiom fold_left_cons :
        forall f acc x l.
        fold_left f acc (cons x l) = fold_left f (f acc x) l *)

  (*@ function fold_right (f: 'a -> 'b -> 'b) (s: 'a t) (acc: 'b) : 'b *)
  (** [fold_right f s acc] is [f s[1] (f s[2] (... (f s[n] acc) ...))] where [n]
      is the length of [s]. *)

  (*@ axiom fold_right_empty :
        forall acc f.
        fold_right f empty acc = acc *)

  (*@ axiom fold_right_cons :
        forall acc f x l.
        fold_right f (cons x l) acc = f x (fold_right f l acc) *)

  (*@ predicate permut (s1 : 'a t) (s2 : 'a t) *)
  (** [permut a b] is true iff [a] and [b] contain the same elements with the
      same number of occurrences *)

  (*@ axiom permut_mem :
        forall s1 s2.
        permut s1 s2 ->
        (forall x. mem x s1 <-> mem x s2) *)

  (*@ predicate permut_sub (s1 : 'a t) (s2 : 'a t) (i : integer) (j : integer) *)
  (** [permut_sub a b lo hi] is true iff the segment [[lo..hi]] is a permutation
      of the segment [[lo..hi]] and values outside of the interval are equal. *)

  (*@ axiom permut_sub_def :
        forall s1 s2 i j.
        permut s1[i..j] s2[i..j] ->
        s1[..i] = s2[..i] ->
        s1[j..] = s2[j..] ->
        permut_sub s1 s2 i j
  *)
end

(** {1 Bags} *)

module Bag : sig
  (*@ type 'a t = 'a bag *)
  (** An alias for ['a bag]. *)

  (*@ function multiplicity (x: 'a) (b: 'a t): integer *)
  (** [multiplicity x b] is the number of occurrences of [x] in [s]. *)

  (*@ axiom well_formed :
        forall b x.
        multiplicity x b >= 0 *)

  (*@ function empty : 'a t *)
  (** [empty] is the empty bag. *)

  (*@ axiom empty_mult :
        forall x.
        multiplicity x empty = 0 *)

  (*@ function init (f : 'a -> integer) : 'a t *)
  (** [init f] creates a bag where every element [x] has multiplicity
      [max 0 (f x)]. *)

  (*@ axiom init_axiom :
        forall f x.
        max 0 (f x) = multiplicity x (init f) *)

  (*@ predicate mem (x: 'a) (b: 'a t) *)
  (** [mem x b] holds iff [b] contains [x] at least once. *)

  (*@ axiom mem_def :
        forall x b.
        mem x b <-> multiplicity x b > 0 *)

  (*@ function add (x: 'a) (b: 'a t) : 'a t *)
  (** [add x b] is [b] when an occurence of [x] was added. *)

  (*@ axiom add_mult_x :
        forall b x.
        multiplicity x (add x b) = 1 + multiplicity x b *)

  (*@ axiom add_mult_neg_x :
        forall x y b. x <> y ->
        multiplicity y (add x b) = (multiplicity y b) *)

  (*@ function singleton (x: 'a) : 'a t *)
  (** [singleton x] is a bag containing one occurence of [x]. *)

  (*@ axiom singleton_def :
        forall x.
        singleton x = add x empty *)

  (*@ function remove (x: 'a) (b: 'a t) : 'a t *)
  (** [remove x b] is [b] where an occurence of [x] was removed. *)

  (*@ axiom remove_mult_x :
        forall b x.
        multiplicity x (remove x b) = max 0 (multiplicity x b - 1) *)

  (*@ axiom remove_mult_neg_x :
        forall x y b.
        x <> y ->
        multiplicity y (remove x b) = multiplicity y b *)

  (*@ function union (b b': 'a t) : 'a t *)
  (** [union b b'] is a bag [br] where for all element [x],
      [occurences x br = max (occurences x b) (occurences x b')]. *)

  (*@ axiom union_all :
        forall b b' x.
        max (multiplicity x b) (multiplicity x b') =
          multiplicity x (union b b') *)

  (*@ function sum (b b': 'a t) : 'a t *)
  (** [sum b b'] is a bag [br] where for all element [x],
      [occurences x br = (occurences x b) + (occurences x b')]. *)

  (*@ axiom sum_all :
        forall b b' x.
        multiplicity x b + multiplicity x b' =
          multiplicity x (sum b b') *)

  (*@ function inter (b b': 'a t) : 'a t *)
  (** [inter b b'] is a bag [br] where for all element [x],
      [occurences x br = min (occurences x b) (occurences x b')]. *)

  (*@ axiom inter_all :
        forall b b' x.
        min (multiplicity x b) (multiplicity x b') =
        multiplicity x (inter b b') *)

  (*@ predicate disjoint (b b': 'a t) *)
  (** [disjoint b b'] holds iff [b] and [b'] have no element in common. *)

  (*@ axiom disjoint_def :
        forall b b'.
        disjoint b b' <->
          (forall x. mem x b -> not (mem x b'))*)

  (*@ function diff (b b': 'a t) : 'a t *)
  (** [diff b b'] is a bag [br] where for all element [x],
      [occurences x br = max 0 (occurences x b - occurences x b')]. *)

  (*@ axiom diff_all :
        forall b b' x.
        max 0 (multiplicity x b - multiplicity x b') =
          multiplicity x (diff b b') *)

  (*@ predicate subset (b b': 'a t) *)
  (** [subset b b'] holds iff for all element [x],
      [multiplicity x b <= multiplicity x b']. *)

  (*@ axiom subset_def :
       forall b b'.
       subset b b' <->
         (forall x. multiplicity x b <= multiplicity x b') *)

  (*@ function filter (f: 'a -> bool) (b: 'a t) : 'a t *)
  (** [filter f b] is the bag of all elements in [b] that satisfy [f]. *)

  (*@ axiom filter_mem :
        forall b x f.
        f x ->
        multiplicity x (filter f b) = multiplicity x b *)

  (*@ axiom filter_mem_neg :
        forall b x f.
        not (f x) ->
        multiplicity x (filter f b) = 0 *)

  (*@ function cardinal (b: 'a t) : integer *)
  (** [cardinal b] is the total number of elements in [b], all occurrences being
      counted. *)

  (*@ predicate finite (b : 'a t) *)
  (** [finite b] holds when there are a finite number of elements in bag [b] *)

  (*@ axiom finite_def :
        forall b.
        finite b <->
          (exists s. forall x.
           mem x b ->
           Sequence.mem x s) *)

  (*@ axiom card_nonneg :
        forall b.
        cardinal b >= 0 *)

  (*@ axiom card_empty : cardinal empty = 0 *)

  (*@ axiom card_singleton :
        forall x.
        cardinal (singleton x) = 1 *)

  (*@ axiom card_union :
        forall b1 b2.
        finite b1 ->
        finite b2 ->
        cardinal (union b1 b2) = cardinal b1 + cardinal b2 *)

  (*@ axiom card_add :
       forall x b.
       finite b ->
       cardinal (add x b) = cardinal b + 1 *)

  (*@ axiom card_map :
       forall f b.
       finite b ->
       cardinal (filter f b) <= cardinal b *)

  (*@ function of_seq (s: 'a Sequence.t) : 'a t *)
  (** [of_seq s] returns a bag where the multiplicity of each element is the
      same as in [s] *)

  (*@ axiom of_seq_multiplicity :
        forall s x.
        Sequence.multiplicity x s = multiplicity x (of_seq s) *)
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

  (*@ axiom empty_mem :
      forall x.
      not (mem x empty) *)

  (*@ function add (x: 'a) (s: 'a t) : 'a t *)
  (** [add x s] is [s ∪ {x}]. *)

  (*@ axiom add_mem :
        forall s x.
        mem x (add x s) *)

  (*@ axiom add_mem_neq :
        forall s x y.
        x <> y ->
        (mem x s <-> mem x (add y s)) *)

  (*@ function singleton (x: 'a) : 'a t *)
  (** [singleton x] is [{x}]. *)

  (*@ function remove (x: 'a) (s: 'a t) : 'a t *)
  (** [remove x s] is [s ∖ {x}]. *)

  (*@ axiom remove_mem :
        forall s x.
        not (mem x (remove x s)) *)

  (*@ axiom remove_mem_neq :
        forall s x y.
        x <> y ->
        (mem x s <-> mem x (remove y s)) *)

  (*@ function union (s s': 'a t) : 'a t *)
  (** [union s s'] is [s ∪ s']. *)

  (*@ axiom union_mem :
        forall s s' x.
        (mem x s || mem x s') ->
        mem x (union s s') *)

  (*@ axiom union_mem_neg :
      forall s s' x.
      not (mem x s) ->
      not (mem x s') ->
      not (mem x (union s s')) *)

  (*@ function inter (s s': 'a t) : 'a t *)
  (** [inter s s'] is [s ∩ s']. *)

  (*@ axiom inter_mem :
        forall s s' x.
        mem x s ->
        mem x s' ->
        mem x (inter s s') *)

  (*@ axiom inter_mem_neq :
        forall s s' x.
        not (mem x s || mem x s') ->
        not mem x (inter s s') *)

  (*@ predicate disjoint (s s': 'a t) *)
  (** [disjoint s s'] is [s ∩ s' = ∅]. *)

  (*@ axiom disjoint_def :
        forall s s'.
        disjoint s s' <-> inter s s' = empty *)

  (*@ function diff (s s': 'a t) : 'a t *)
  (** [diff s s'] is [s ∖ s']. *)

  (*@ axiom diff_mem :
        forall s s' x.
        mem x s' ->
        not (mem x (diff s s')) *)

  (*@ axiom diff_mem_fst :
        forall s s' x.
        not (mem x s') ->
        (mem x s <-> mem x (diff s s')) *)

  (*@ predicate subset (s s': 'a t) *)
  (** [subset s s'] is [s ⊂ s']. *)

  (*@ axiom subset_def :
        forall s s'.
        subset s s' <->
          (forall x. mem x s -> mem x s') *)

  (*@ function map (f: 'a -> 'b) (s: 'a t) : 'b t *)
  (** [map f s] is a fresh set which elements are [f x1 ... f xN], where
      [x1 ... xN] are the elements of [s]. *)

  (*@ axiom set_map :
        forall f s x.
        mem x (map f s) <->
          (exists y. f y = x && mem y s) *)

  (*@ function partition (f: 'a -> bool) (s: 'a t) : ('a t * 'a t) *)
  (** [partition f s] is the pair of sets [(s1, s2)], where [s1] is the set of
      all the elements of [s] that satisfy the predicate [f], and [s2] is the
      set of all the elements of [s] that do not satisfy [f]. *)

  (*@ axiom partition_l_mem :
        forall f s x p1 p2.
        mem x s ->
        f x ->
        partition f s = (p1, p2) ->
        mem x p1 *)

  (*@ axiom partition_r_mem :
        forall f s x p1 p2.
        mem x s ->
        not f x ->
        partition f s = (p1, p2) ->
        mem x p2 *)

  (*@ function cardinal (s: 'a t) : integer *)
  (** [cardinal s] is the number of elements in [s]. *)

  (*@ predicate finite (s : 'a t) *)

  (*@ axiom finite_def :
        forall s.
        finite s <->
          (exists seq. forall x.
           mem x s ->
           Sequence.mem x seq) *)

  (*@ axiom cardinal_nonneg :
        forall s.
        cardinal s >= 0 *)

  (*@ axiom cardinal_empty : cardinal empty = 0 *)

  (*@ axiom cardinal_remove :
        forall s x.
        finite s ->
        if mem x s
         then cardinal (remove x s) = cardinal s - 1
         else cardinal (remove x s) = cardinal s *)

  (*@ axiom cardinal_add :
        forall s x. finite s ->
        if mem x s
          then cardinal (add x s) = cardinal s
          else cardinal (add x s) = cardinal s + 1 *)

  (*@ function of_seq (s: 'a Sequence.t) : 'a t *)
  (** [of_seq] returns a set where each element also belongs to [s] *)

  (*@ axiom of_seq_mem :
        forall s.
        (forall x. mem x (of_seq s) <-> Sequence.mem x s) *)

  (*@ function to_seq (s: 'a t) : 'a Sequence.t *)
  (** [of_seq] returns a sequence where each element also belongs to [s]. The
      order in which they appear is unspecified. *)

  (*@ axiom to_seq_mem :
        forall s.
        finite s ->
        (forall x. mem x s <->
          Sequence.multiplicity x (to_seq s) = 1) *)

  (*@ function fold (f : 'a -> 'b) (m : 'b -> 'b -> 'b) (s : 'a t) (acc : 'b) : 'b *)
  (** [fold f m s acc] is equal to [m (... (m (m acc (f x)) (f y)) ...) (f z)],
      where [x, y, ... z] are the elements of [s]. The order in which the values
      are passed to [m] is unspecified *)

  (*@ axiom fold_def :
        forall f : ('a -> 'b).
        forall m s acc.
        finite s ->
        comm_monoid m acc ->
        fold f m s acc =
          Sequence.fold_right (fun x acc -> m (f x) acc) (to_seq s) acc *)
end

(*@ function ( [->] ) (f: 'a -> 'b) (x:'a) (y: 'b) : 'a -> 'b *)
(** [f[x -> y]] returns a new function where [x] maps to [y] and all other
    values have the same mapping as [f] *)

(*@ axiom map_set_def :
      forall f : ('a -> 'b).
      forall x : 'a.
      forall y : 'b.
      f[x -> y] = fun arg -> if arg = x then y else f x *)

module Map : sig
  (** Maps from keys of type ['a] to values of type ['b] are represented by
      Gospel functions of type ['a -> 'b]. *)

  (*@ type ('a, 'b) t = ('a, 'b) map *)

  (*@ function domain (default : 'b) (m : ('a, 'b) t) : 'a Set.t *)
  (** [domain default m] returns the set of values [x] where [m x <> default].
  *)

  (*@ axiom domain_mem :
        forall x m default.
        m x <> default ->
        Set.mem x (domain default m) *)
end
