(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(* This file contains the GOSPEL standard library.
   It is automatically opened.

   The following are built-in in GOSPEL:

   type unit
   type string
   type float
   type bool
   type integer

   type 'a option
   function None: 'a option
   function Some (x: 'a) : 'a option

   type 'a list
   function ([]): 'a list
   function (::) (x: 'a) (l: 'a list) : 'a list

   predicate (=) (x y: 'a)
*)

(** Arithmetic

   The type [integer] is built-in. This is the type of arbitrary precision integers,
   not to be confused with OCaml's type [int] (machine integers).

   There is a coercion from type [int] to type [integer], so that GOSPEL specifications
   can be written using type [integer] only, and yet use OCaml's variables of type [int].
*)

(*@ function (+)   (x y: integer) : integer *)
(*@ function (-)   (x y: integer) : integer *)
(*@ function ( * ) (x y: integer) : integer *)
(*@ function (/)   (x y: integer) : integer *)
(*@ function mod   (x y: integer) : integer *) (* TODO allow infix in the parser*)
(*@ function (-_)  (x: integer) : integer *)
(*@ predicate (>)  (x y: integer) *)
(*@ predicate (>=) (x y: integer) *)
(*@ predicate (<)  (x y: integer) *)
(*@ predicate (<=) (x y: integer) *)

type int

(*@ function integer_of_int (x: int) : integer *)
(*@ coercion *)

(*@ function abs (x:integer) : integer = if x >= 0 then x else -x *)

(*@ function min (x y : integer) : integer
    = if x <= y then x else y *)

(*@ function max (x y : integer) : integer
    = if x <= y then y else x *)

(*@ function succ (x: integer) : integer = x + 1 *)
(*@ function pred (x: integer) : integer = x - 1 *)

(*@ function max_int : integer *)
(*@ function min_int : integer *)


(** Tuples *)

(*@ function fst (p: 'a * 'b) : 'a *)
(*@ function snd (p: 'a * 'b) : 'b *)

(** References *)

type 'a ref
(*@ ephemeral *)
(*@ mutable model contents: 'a *)

(*@ function (!_) (r: 'a ref) : 'a = r.contents *)

(** Sequences

    They are used in the following to model lists and arrays, and possibly other
    data structures (queues, etc.).
*)

(*@ type 'a seq *)

(*@ function length (s: 'a seq): integer *)

(*@ function ([_]) (s: 'a seq) (i:integer): 'a *)

(*@ predicate (==) (s1 s2: 'a seq) =
      length s1 = length s2 &&
      forall i. 0 <= i < length s1 -> s1[i] = s2[i] *)

(*@ function ([_.._]) (s: 'a seq) (i1: integer) (i2: integer): 'a seq *)
(*@ function ([_..]) (s: 'a seq) (i: integer): 'a seq *)
(*@ function ([.._]) (s: 'a seq) (i: integer): 'a seq *)

(*@ function empty: 'a seq *)

(*@ function (++) (s1: 'a seq) (s2: 'a seq): 'a seq *)

module Seq : sig

  (* re-export type t and functions length and [_], so that we can refer to them
     using qualified identifiers (Seq.t, Seq.len, and Seq.get, respectively). *)
  (*@ type 'a t = 'a seq *)
  (*@ function len (s: 'a seq): integer = length s *)
  (*@ function get (s: 'a seq) (i: integer) : 'a = s[i] *)

  (*@ function create (x: integer) (f: integer -> 'a): 'a seq *)
  (*@ axiom create_len : forall n, f. n >= 0 ->
        length (create n f) = n *)
  (*@ axiom create_def : forall n, f. n >= 0 ->
        forall i. 0 <= i < n -> (create n f)[i] = f i *)

  (* TODO : DO WE WANT SOMETHING LIKE THIS ? *)
  (*@ function create (n: integer) (f: integer -> 'a) : 'a seq *)
  (*@ requires 0 <= n
      ensures  length result = n
      ensures  forall i. 0 <= i < n -> result[i] = f i *)

  (*@ function ([<-]) (s: 'a seq) (i: integer) (x: 'a): 'a seq *)

  (*@ function cons (x: 'a) (s: 'a seq): 'a seq *)
  (*@ function snoc (s: 'a seq) (x: 'a): 'a seq *)

  (* FIXME singleton? *)

  (*@ predicate mem (s: 'a seq) (x: 'a) =
        exists i. 0 <= i < length s && s[i] = x *)

  (*@ predicate distinct (s: 'a seq) =
        forall i j. 0 <= i < length s -> 0 <= j < length s ->
        i <> j -> s[i] <> s[j] *)

  (*@ function rev (s: 'a seq) : 'a seq =
        create (length s) (fun i -> s[length s - 1 - i]) *)

  (*@ function map (f: 'a -> 'b) (s: 'a seq) : 'b seq =
        create (length s) (fun i -> f s[i]) *)

  (*@ function rec fold_left (f: 'a -> 'b -> 'a) (acc: 'a) (s: 'b seq) : 'a =
        if length s = 0 then acc
        else fold_left f (f acc s[0]) s[1 ..] *)

  (*@ function rec fold_right (f: 'a -> 'b -> 'b) (s: 'a seq) (acc: 'b) : 'b =
        if length s = 0 then acc
        else f s[0] (fold_right f s[1 ..] acc) *)

  (*@ function hd (s: 'a seq) : 'a = s[0] *)
  (*@ function tl (s: 'a seq) : 'a seq = s[1 ..] *)

  (* hd, tl, rev, mem *)
  (* higher-order: map, fold, exists, forall, find, partition *)
  (* assoc, mem_assoc? split, combine? *)
end

(** Lists

     Type 'a list, [] and (::) constructors are built-in *)

(*@ function seq_of_list (l: 'a list): 'a seq *)
(*@ coercion *)

(** Arrays *)

type 'a array
(*@ ephemeral *)
(*@ mutable model contents: 'a seq *)
(*@ model array_length: integer *)
(*@ invariant array_length = length contents *)

(*@ function elts (a: 'a array): 'a seq = a.contents *)
(*@ coercion *)

module Array : sig

  (* OCaml-like syntax a.(i) is mapped to Array.([_]) *)
  (*@ function ([_]) (a: 'a array) (i: integer) : 'a = Seq.get a i *)

  (*@ function length (a: 'a array) : integer = Seq.len a *)

end

module ArrayPermut : sig

  (*@ predicate permut_sub (a b: 'a array) (i j: integer) *)
  (*@ predicate permut_all (a b: 'a array) *)

end

(** Other OCaml built-in stuff *)

exception Not_found
exception Invalid_argument of string

module Sys : sig

  (*@ function word_size : integer *)

  (*@ function int_size : integer *)

  (*@ function big_endian : bool *)

  (*@ function max_string_length : integer *)

  (*@ function max_array_length : integer *)

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

module Bag : sig

  (*@ type 'a bag *)

  (*@ function nb_occ (x: 'a) (b: 'a bag): integer *)

  (*@ axiom occ_non_negative: forall b: 'a bag, x: 'a.
        nb_occ x b >= 0 *)

  (*@ predicate mem (x: 'a) (b: 'a bag) =
        nb_occ x b > 0 *)

  (*@ predicate eq_bag (a b: 'a bag) =
        forall x:'a. nb_occ x a = nb_occ x b *)

  (*@ axiom bag_extensionality: forall a b: 'a bag.
        eq_bag a b -> a = b *)

  (*@ function empty_bag: 'a bag *)

  (*@ axiom occ_empty: forall x: 'a. nb_occ x empty_bag = 0 *)

  (*@ function singleton (x: 'a) : 'a bag *)

  (*@ axiom occ_singleton: forall x y: 'a.
        nb_occ y (singleton x) = if x = y then 1 else 0 *)

  (*@ function union (x:'a bag) (y:'a bag) : 'a bag *)

  (* axiom occ_union: forall x: 'a, a b: 'a bag.
      nb_occ x (union a b) = nb_occ x a + nb_occ x b *)

    (** add operation *)

  (*@ function add (x: 'a) (b: 'a bag) : 'a bag =
        union (singleton x) b *)

  (** cardinality of bags *)

  (*@ function card (x:'a bag): integer *)

  (*@ axiom card_nonneg: forall x: 'a bag.
        card x >= 0 *)

  (*@ axiom card_empty: card (empty_bag: 'a bag) = 0 *)

  (*@ axiom card_zero_empty: forall x: 'a bag.
        card x = 0 -> x = empty_bag *)

  (*@ axiom card_singleton: forall x:'a.
        card (singleton x) = 1 *)

  (*@ axiom card_union: forall x y: 'a bag.
        card (union x y) = card x + card y *)

  (** bag difference *)

  (*@ function diff (x: 'a bag) (y: 'a bag) : 'a bag *)

  (*@ axiom diff_occ: forall b1 b2: 'a bag, x:'a.
      nb_occ x (diff b1 b2) = max 0 (nb_occ x b1 - nb_occ x b2) *)

  (** arbitrary element *)

  (*@ function choose (b: 'a bag) : 'a *)

  (*@ axiom choose_mem: forall b: 'a bag.
        empty_bag <> b -> mem (choose b) b *)

end

module Set : sig

  (*@ type 'a set *)

  (*@ predicate mem (x: 'a) (s: 'a set) *)

  (*@ function ( {} ) : 'a set *)

  (*@ function ( {:_:} ) (x: 'a) : 'a set *)

  (*@ function union (x:'a set) (y:'a set) : 'a set *)

  (*@ function sum (f:'a -> integer) (x: 'a set) : integer *)

end

module Map : sig

  (* the type ('a, 'b) map is defined internally in GOSPEL and can be
     written as 'a -> 'b *)

  (*@ function ( [<-] ) (m: 'a -> 'b) (x:'a) (y: 'b) : 'a -> 'b *)

  (*@ function ( [_] ) (m: 'a -> 'b) (x: 'a) : 'b *)

end

module Peano : sig
  type t
  (*@ model v: integer *)

  (*@ function int_of_peano (t: t) : integer = t.v *)
  (*@ coercion *)
end
