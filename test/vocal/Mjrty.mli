(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function num (a: string seq) (v: string) (lo hi: integer) : integer *)
(** the number of occurrences of [v] in [a] between index [lo]
    included and index [hi] excluded *)

(*@ axiom num_base:
      forall a v lo hi. hi <= lo -> num a v lo hi = 0 *)
(*@ axiom num_ind:
      forall a v lo hi. 0 <= lo < hi <= Seq.length a ->
      num a v lo hi = (if a[lo] = v then 1 else 0) + num a v (lo+1) hi *)

val mjrty: string array -> string
(** [mjrty a] returns the element of [a] with absolute majority, if any,
    or raises [Not_found] otherwise *)
(*@ r = mjrty a
      requires 1 <= Array.length a
      ensures  2 * num a r 0 (Array.length a) > Array.length a
      raises   Not_found ->
               forall c. 2 * num a c 0 (Array.length a) <= Array.length a *)
