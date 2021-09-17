(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ open Set *)
(*@ open Map *)

type 'a elem

(*@ type 'a uf *)
(*@ mutable model dom : 'a elem set
    mutable model rep : 'a elem -> 'a elem
    mutable model img : 'a elem -> 'a
    invariant forall x: 'a elem. mem x dom -> img x = img (rep x)
    invariant forall x: 'a elem. mem x dom -> rep (rep x) = rep x
    invariant forall x: 'a elem. mem x dom -> mem (rep x) dom *)

(*@ predicate equiv (uf: 'a uf) (x: 'a elem) (y: 'a elem) =
      rep uf x = rep uf y *)

(*@ val create: unit -> 'a uf *)
(*@ uf = create ()
      ensures dom uf = {} *)

val make : 'a -> 'a elem
(*@ e = make [uf: 'a uf] v
      (* requires $ O(1) *)
      modifies uf
      ensures  not (mem e (old (dom uf)))
      ensures  dom uf = Set.add e (old (dom uf))
      ensures  rep uf = (old (rep uf))[e <- e]
      ensures  img uf = (old (img uf))[e <- v]
*)

(** note: in functions find, eq, and get,
    'modifies' accounts for path compression *)

(*@ function alpha : integer -> integer *)

(* @ axiom alpha_is_a_constant_in_practice:
    forall n. 0 <= n <= max_int -> alpha n <= 5 *)

val find : 'a elem -> 'a elem
(*@ r = find [uf: 'a uf] e
      (* requires $ O(alpha(n)) where n = card (dom uf) *)
      (* could be O(1) if we prove (internally) that card dom is bounded e.g. by
         max_int *)
      requires mem e (dom uf)
      modifies uf
      ensures  dom uf = old (dom uf)
      ensures  rep uf = old (rep uf)
      ensures  img uf = old (img uf)
      ensures  r = rep uf e
*)

val eq : 'a elem -> 'a elem -> bool
(*@ b = eq [uf: 'a uf] e1 e2
      (* requires $ O(alpha(n)) where n = card (dom uf) *)
      requires mem e1 (dom uf)
      requires mem e2 (dom uf)
      modifies uf
      ensures  dom uf = old (dom uf)
      ensures  rep uf = old (rep uf)
      ensures  img uf = old (img uf)
      ensures  b <-> rep uf e1 = rep uf e2
*)

val get : 'a elem -> 'a
(*@ v = get [uf: 'a uf] e
      (* requires $ O(alpha(n)) where n = card (dom uf) *)
      requires mem e (dom uf)
      modifies uf
      ensures  dom uf = old (dom uf)
      ensures  rep uf = old (rep uf)
      ensures  img uf = old (img uf)
      ensures  v = img uf e
*)

val set : 'a elem -> 'a -> unit
(*@ set [uf: 'a uf] e v
      (* requires $ O(alpha(n)) where n = card (dom uf) *)
      requires mem e (dom uf)
      modifies uf
      ensures  dom uf = old (dom uf)
      ensures  rep uf = old (rep uf)
      ensures  forall x. img uf x = if equiv uf x e then v else old (img uf x)
*)

val union : 'a elem -> 'a elem -> unit
(*@ union [uf: 'a uf] e1 e2
      (* requires $ O(alpha(n)) where n = card (dom uf) *)
      requires mem e1 (dom uf)
      requires mem e2 (dom uf)
      modifies uf
      ensures  dom uf = old (dom uf)
      ensures  exists r. (r = old (rep uf e1) || r = old (rep uf e2)) &&
           forall x. mem x uf.dom ->
           rep uf x = (if old (equiv uf x e1 || equiv uf x e2) then r
                      else old (rep uf x))
        && img uf x = if old (equiv uf x e1 || equiv uf x e2) then img uf r
                      else old (img uf x)
*)

(*@ val join: 'a uf -> 'a uf -> unit *)
(*@ join uf1 uf2
    (*? requires disjoint (dom uf1) (dom uf2) *)
    modifies uf1
    consumes uf2
    ensures  dom uf1 = old (dom uf1) `union` old (dom uf2)
    ensures  forall x.
      rep uf1 x = if mem x (old (dom uf1)) then old (rep uf1 x)
                                           else old (rep uf2 x)
    ensures  forall x.
      img uf1 x = if mem x (old (dom uf1)) then old (img uf1 x)
                                             else old (img uf2 x)
*)
