(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Vectors (aka resizable arrays, growing arrays, dynamic arrays, etc.)

    This module implements arrays that automatically expand as necessary. Its
    implementation uses a traditional array and replaces it with a larger array
    when needed (and elements are copied from the old array to the new one). The
    current implementation doubles the capacity when growing the array (and
    shrinks it whenever the number of elements comes to one fourth of the
    capacity).

    The unused part of the internal array is filled with a dummy value, which is
    user-provided at creation time (and referred to below as ``the dummy
    value''). Consequently, vectors do not retain pointers to values that are
    not used anymore after a shrinking.

    Vectors provide an efficient implementation of stacks, with a better
    locality of reference than list-based implementations (such as standard
    library {!Stack}). A stack interface is provided, similar to that of
    {!Stack} (though {!Vector.push} have arguments in the other way round).
    Inserting [n] elements with {!Vector.push} has overall complexity O(n) i.e.
    each insertion has amortized constant time complexity. *)

type 'a t
(** The polymorphic type of vectors. This is a mutable data type. *)
(*@ mutable model view: 'a sequence
    with self
    invariant Sequence.length self.view <= Sys.max_array_length *)

(** {2 Operations proper to vectors, or with a different type and/or semantics
    than those of module [Array]} *)

val create : ?capacity:int -> dummy:'a -> 'a t
(** [create] returns a fresh vector of length [0]. All the elements of this new
    vector are initially physically equal to [dummy] (in the sense of the [==]
    predicate). When [capacity] is omitted, it defaults to 0. *)
(*@ a = create ?capacity ~dummy
      requires let capacity = match capacity with
                 | None -> 0 | Some c -> c in
               0 <= capacity <= Sys.max_array_length
      ensures  Sequence.length a.view = 0 *)

val make : ?dummy:'a -> int -> 'a -> 'a t
(** [make dummy n x] returns a fresh vector of length [n] with all elements
    initialized with [x]. If [dummy] is omitted, [x] is also used as a dummy
    value for this vector. *)
(*@ a = make ?dummy n x
      requires 0 <= n <= Sys.max_array_length
      ensures  Sequence.length a.view = n
      ensures  forall i: integer. 0 <= i < n -> a.view[i] = x *)

val init : dummy:'a -> int -> (int -> 'a) -> 'a t
(** [init n f] returns a fresh vector of length [n], with element number [i]
    initialized to the result of [f i]. In other terms, [init n f] tabulates the
    results of [f] applied to the integers [0] to [n-1].

    Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length]. *)
(*@ a = init ~dummy n f
      requires 0 <= n <= Sys.max_array_length
      ensures  Sequence.length a.view = n
      ensures  forall i: int. 0 <= i < n -> a.view[i] = f i *)

val resize : 'a t -> int -> unit
(** [resize a n] sets the length of vector [a] to [n].

    The elements that are no longer part of the vector, if any, are internally
    replaced by the dummy value of vector [a], so that they can be garbage
    collected when possible.

    Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length]. *)
(*@ resize a n
      checks   0 <= n <= Sys.max_array_length
      modifies a
      ensures  Sequence.length a.view = n
      ensures  forall i. 0 <= i < min (Sequence.length (old a.view)) n ->
                 a.view[i] = (old a.view)[i] *)

(** {2 Array interface} *)

val clear : 'a t -> unit
(** Discard all elements from a vector. This is equivalent to setting the size
    to 0 with [resize]. *)
(*@ clear a
      modifies a
      ensures Sequence.length a.view = 0 *)

val is_empty : 'a t -> bool
(** Return [true] if the given vector is empty, [false] otherwise. *)
(*@ r = is_empty a
      ensures r <-> Sequence.length a.view = 0 *)

val length : 'a t -> int
(** Return the length (number of elements) of the given vector. Note: the number
    of memory words occupied by the vector can be larger. *)
(*@ n = length a
      ensures n = Sequence.length a.view *)

val get : 'a t -> int -> 'a
(** [get a n] returns the element number [n] of vector [a]. The first element
    has number [0]. The last element has number [length a - 1].

    Raise [Invalid_argument "Vector.get"] if [n] is outside the range [0] to
    [length a - 1]. *)
(*@ x = get a i
      requires 0 <= i < Sequence.length a.view
      ensures  x = a.view[i] *)

val set : 'a t -> int -> 'a -> unit
(** [set a n x] modifies aector [a] in place, replacing element number [n] with
    [x].

    Raise [Invalid_argument "Vector.set"] if [n] is outside the range 0 to
    [length a - 1]. *)
(*@ set a i x
      requires 0 <= i < Sequence.length a.view
      modifies a
      ensures Sequence.length a.view = Sequence.length (old a).view
      ensures  a.view[i] = x
      ensures  forall j. 0 <= j < Sequence.length a.view -> j <> i ->
                 a.view[j] = (old a).view[j] *)

val sub : 'a t -> int -> int -> 'a t
(** [sub a start len] returns a fresh vector of length [len], containing the
    elements number [start] to [start + len - 1] of vector [a]. *)
(*@ r = sub a ofs n
      requires 0 <= ofs /\ 0 <= n /\ ofs + n <= Sequence.length a.view
      ensures  Sequence.length r.view = n
      ensures  forall i. 0 <= i < n -> r.view[i] = a.view[ofs + i] *)

val fill : 'a t -> int -> int -> 'a -> unit
(** [fill a ofs len x] modifies the vector [a] in place, storing [x] in elements
    number [ofs] to [ofs + len - 1].

    Raise [Invalid_argument "Vector.fill"] if [ofs] and [len] do not designate a
    valid subvector of [a]. *)
(*@ fill a ofs n x
      requires 0 <= ofs /\ 0 <= n /\ ofs + n <= Sequence.length a.view
      modifies a
      ensures  forall i. (0 <= i < ofs \/ ofs + n <= i < Sequence.length a.view) ->
                 a.view[i] = (old a).view[i]
      ensures  forall i. ofs <= i < ofs + n -> a.view[i] = x *)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [blit a1 o1 a2 o2 len] copies [len] elements from vector [a1], starting at
    element number [o1], to vector [a2], starting at element number [o2]. It
    works correctly even if [a1] and [a2] are the same vector, and the source
    and destination chunks overlap.

    Raise [Invalid_argument "Vector.blit"] if [o1] and [len] do not designate a
    valid subvector of [a1], or if [o2] and [len] do not designate a valid
    subvector of [a2]. *)
(*@ blit a1 ofs1 a2 ofs2 n
      requires 0 <= n
      requires 0 <= ofs1 /\ ofs1 + n <= Sequence.length a1.view
      requires 0 <= ofs2 /\ ofs2 + n <= Sequence.length a2.view
      modifies a2
      ensures  forall i. (0 <= i < ofs2 \/ ofs2 + n <= i < Sequence.length a2.view) ->
                         a2.view[i] = (old a2).view[i]
      ensures  forall i. ofs2 <= i < ofs2 + n ->
                         a2.view[i] = (old a1).view[ofs1 + i - ofs2] *)

val append : 'a t -> 'a t -> 'a t
(** [append a1 a2] returns a fresh vector containing the concatenation of the
    elements of [a1] and [a2].

    It works correctly even if [a1] and [a2] are the same vector. *)
(*@ a3 = append a1 a2
      requires Sequence.length a1.view + Sequence.length a2.view <= Sys.max_array_length
      ensures  Sequence.length a3.view = Sequence.length a1.view + Sequence.length a2.view
      ensures  forall i. 0 <= i < Sequence.length a1.view -> a3.view[i] = a1.view[i]
      ensures  forall i. 0 <= i < Sequence.length a2.view ->
                 a3.view[Sequence.length a1.view + i] = a2.view[i] *)

val merge_right : 'a t -> 'a t -> unit
(** [merge_right a1 a2] moves all elements of [a2] to the end of [a1]. Empties
    [a2]. Assumes [a1] and [a2] to be disjoint. *)
(*@ merge_right a1 a2
      requires Sequence.length a1.view + Sequence.length a2.view <= Sys.max_array_length
      modifies a1, a2
      ensures  Sequence.length a2.view = 0
      ensures  Sequence.length a1.view = Sequence.length (old a1).view + Sequence.length (old a2).view
      ensures  forall i. 0 <= i < Sequence.length (old a1).view ->
                 a1.view[i] = (old a1).view[i]
      ensures  forall i. 0 <= i < Sequence.length (old a2).view ->
                 a1.view[Sequence.length (old a1).view + i] = (old a2).view[i] *)

(* requires disjoint a1 a2 FIXME: disjoint is undefined *)

val map : dummy:'b -> 'a t -> ('a -> 'b) -> 'b t
(** [map f a] applies function [f] to all the elements of [a], and builds a
    fresh vector with the results returned by [f].

    Note: the dummy value of the returned vector is obtained by applying [f] to
    the dummy value of [a]. If this is not what you want, first create a new
    vector and then fill it with the value [f (get a 0)], [f (get a 1)], etc. *)
(*@ a2 = map ~dummy a1 f
      ensures  Sequence.length a2.view = Sequence.length a1.view
      ensures  forall i. 0 <= i < Sequence.length a1.view -> a2.view[i] = f a1.view[i] *)

val mapi : dummy:'b -> 'a t -> (int -> 'a -> 'b) -> 'b t
(** Same as {!Vector.map}, but the function is applied to the index of the
    element as first argument, and the element itself as second argument.

    Note: the dummy value of the returned vector is obtained by applying [f 0]
    to the dummy value of [a]. *)
(*@ a2 = mapi ~dummy a1 f
      ensures  Sequence.length a2.view = Sequence.length a1.view
      ensures  forall i: int. 0 <= i < Sequence.length a1.view ->
               a2.view[i] = f i a1.view[i] *)

val copy : 'a t -> 'a t
(** [copy a] returns a copy of [a], that is, a fresh vector containing the same
    elements as [a]. *)
(*@ a2 = copy a1
      ensures  Sequence.length a2.view = Sequence.length a1.view
      ensures  forall i. 0 <= i < Sequence.length a1.view -> a2.view[i] = a1.view[i] *)

val fold_left : 'b t -> ('a -> 'b -> 'a) -> 'a -> 'a
(** [fold_left f x a] computes
    [f (... (f (f x (get a 0)) (get a 1)) ...) (get a (n-1))], where [n] is the
    length of the vector [a]. *)
(*@ r = fold_left a f acc
      ensures  r = Sequence.fold_left f acc a.view *)

val fold_right : 'b t -> ('b -> 'a -> 'a) -> 'a -> 'a
(** [fold_right f a x] computes
    [f (get a 0) (f (get a 1) ( ... (f (get a (n-1)) x) ...))], where [n] is the
    length of the vector [a]. *)
(*@ r = fold_right a f acc
      ensures  r = Sequence.fold_right f a.view acc *)

(* val iter : ('a -> unit) -> 'a t -> unit
 * (\** [iter f a] applies function [f] in turn to all
 *    the elements of [a].  It is equivalent to
 *    [f (get a 0); f (get a 1); ...; f (get a (Sequence.length a - 1))]. *\)
 * (\*@ iter f a
 *       equivalent "for i = 0 to length a - 1 do f (get a i) done" *\)
 *
 * val iteri : (int -> 'a -> unit) -> 'a t -> unit
 * (\** Same as {!Vector.iter}, but the
 *    function is applied to the index of the element as first argument,
 *    and the element itself as second argument. *\)
 * (\*@ iteri f a
 *       equivalent "for i = 0 to length a - 1 do f i (get a i) done" *\) *)

(** {2 Stack interface}

    Contrary to standard library's {!Stack}, module {!Vector} uses less space
    (between N and 2N words, instead of 3N) and has better data locality. *)

val push : 'a t -> 'a -> unit
(** [push a x] appends [x] at the end of vector [a], i.e., increases the size of
    [a] by one and stores [x] at the rightmost position.

    Note: the order of the arguments is not that of {!Stack.push}. *)
(*@ push a x
      requires Sequence.length a.view < Sys.max_array_length
      modifies a
      ensures  Sequence.length a.view = Sequence.length (old a.view) + 1
      ensures  a.view[Sequence.length a.view - 1] = x
      ensures  forall i. 0 <= i < Sequence.length (old a.view) ->
                 a.view[i] = (old a).view[i] *)

exception Empty

val pop : 'a t -> 'a
(** [pop a] removes and returns the rightmost element in vector [a], or raises
    [Empty] if the stack is empty. *)
(*@ x = pop a
      modifies a
      raises   Empty -> Sequence.length a.view = Sequence.length (old a).view = 0
      ensures  Sequence.length a.view = Sequence.length (old a).view - 1
      ensures  x = (old a).view[Sequence.length a.view]
      ensures  forall i. 0 <= i < Sequence.length a.view ->
                 a.view[i] = (old a).view[i] *)

val pop_opt : 'a t -> 'a option
(** similar to [pop], but with an option instead of an exception *)
(*@ r = pop_opt a
      modifies a
      ensures  match r with
               | None   -> Sequence.length a.view = Sequence.length (old a).view = 0
               | Some x -> Sequence.length a.view = Sequence.length (old a).view - 1 /\
                           x = (old a).view[Sequence.length a.view] /\
                           forall i. 0 <= i < Sequence.length a.view ->
                                     a.view[i] = (old a).view[i] *)

val top : 'a t -> 'a
(** [top a] returns the rightmost element in vector [a], or raises [Empty] if
    the vector is empty. *)
(*@ x = top a
      requires 0 < Sequence.length a.view
      ensures  x = a.view[Sequence.length a.view - 1] *)

val top_opt : 'a t -> 'a option
(** similar to [top], but with an option instead of an exception *)
(*@ r = top_opt a
      ensures  match r with
               | None   -> Sequence.length a.view = 0
               | Some x -> x = a.view[Sequence.length a.view - 1] *)

(** {2 Conversions to/from arrays and lists} *)

(*
val to_list : 'a t -> 'a list
(** [to_list a] returns the list of all the elements of [a]. *)

val of_list: dummy:'a -> 'a list -> 'a t
(** [of_list dummy l] returns a fresh vector containing the elements of [l]. *)

val to_array: 'a t -> 'a array
(** [to_array a] returns the array of all the elements of [a]. *)

val of_array: dummy:'a -> 'a array -> 'a t
(** [of_array dummy a] returns a fresh vector containing the elements of [a]. *)
*)

(** {2 Only if you know what you are doing...} *)

(*
val unsafe_resize: 'a t -> int -> unit
val unsafe_get : 'a t -> int -> 'a
val unsafe_set : 'a t -> int -> 'a -> unit
*)
