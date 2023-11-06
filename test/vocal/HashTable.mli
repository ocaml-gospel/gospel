(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module type HashedType = sig
  type t

  (*@ predicate equiv (x: t) (y: t) *)
  (*@ axiom refl : forall x: t. equiv x x *)
  (*@ axiom sym  : forall x y: t. equiv x y -> equiv y x *)
  (*@ axiom trans: forall x y z: t. equiv x y -> equiv y z -> equiv x z *)
  val equal : t -> t -> bool

  (*@ b = equal x y
      ensures b <-> equiv x y *)
  (*@ function hash_f (x: t) : integer *)
  (*@ axiom compatibility: forall x y: t. equiv x y -> hash_f x = hash_f y *)
  val hash : t -> int
  (*@ h = hash x
      ensures h = hash_f x *)
end

module Make (K : HashedType) : sig
  type key = K.t
  type 'a table
  (*@ ephemeral
      mutable model dom : key set
      mutable model view: key -> 'a list
      with self
      invariant forall x y: key. Set.mem x self.dom -> Set.mem y self.dom -> K.equiv x y -> x = y
      invariant forall k: key. not (Set.mem k self.dom) -> self.view k = [] *)

  type 'a t = 'a table

  val create : int -> 'a t
  (*@ h = create n
    requires n >= 0
    ensures  forall k: key. h.view k = [] *)

  val clear : 'a t -> unit
  (*@ clear h
    modifies h
    ensures  forall k: key. h.view k = [] *)

  val reset : 'a t -> unit
  (*@ reset h
    modifies h
    ensures  forall k: key. h.view k = [] *)

  val copy : 'a t -> 'a t
  (*@ h2 = copy h1
    ensures  forall k: key. h2.view k = h1.view k *)

  (*@ function pop (h: 'a t) : integer =
    Set.fold (fun k c -> List.length (h.view k) + c) h.dom 0 *)

  val population : 'a t -> int
  (*@ n = population h
    ensures n = pop h *)

  val length : 'a t -> int
  (*@ n = length h
    ensures n = pop h *)

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  type statistics = {
    num_bindings : int;
    num_buckets : int;
    max_bucket_length : int;
    bucket_histogram : int array;
  }

  val stats : 'a t -> statistics
  val add : 'a t -> key -> 'a -> unit
  (*@ add h k v
    modifies h
    ensures  forall k': key.
             h.view k = if K.equiv k' k then v :: old (h.view k')
                        else old (h.view k') *)

  (*@ function tail (l: 'a list) : 'a list =
        match l with [] -> [] | _ :: s -> s*)

  val remove : 'a t -> key -> unit
  (*@ remove h k
    modifies h
    ensures  forall k': key.
             h.view k = if K.equiv k' k then tail (old (h.view k'))
                        else old (h.view k') *)

  val find : 'a t -> key -> 'a option
  (*@ r = find h k
    ensures r = match h.view k with [] -> None | x :: _ -> Some x*)

  val find_all : 'a t -> key -> 'a list
  (*@ l = find_all h k
    ensures l = h.view k *)

  val replace : 'a t -> key -> 'a -> unit
  (*@ replace h k v
    modifies h
    ensures  forall k': key.
             h.view k = if K.equiv k' k then v :: tail (old (h.view k))
                        else old (h.view k') *)

  val mem : 'a t -> key -> bool
  (*@ b = mem h k
    ensures b <-> h.view k <> [] *)
end
