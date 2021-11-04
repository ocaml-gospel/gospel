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
      uf.rep x = uf.rep y *)

(*@ val create: unit -> 'a uf *)
(*@ uf = create ()
      ensures uf.dom = {} *)

val make : 'a -> 'a elem
(*@ e = make [uf: 'a uf] v
      (* requires $ O(1) *)
      modifies uf
      ensures  not (mem e (old uf.dom))
      ensures  uf.dom = Set.add e (old uf.dom)
      ensures  uf.rep = (old uf.rep)[e -> e]
      ensures  uf.img = (old uf.img)[e -> v]
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
      requires mem e uf.dom
      modifies uf
      ensures  uf.dom = old uf.dom
      ensures  uf.rep = old uf.rep
      ensures  uf.img = old uf.img
      ensures  r = uf.rep e
*)

val eq : 'a elem -> 'a elem -> bool
(*@ b = eq [uf: 'a uf] e1 e2
      (* requires $ O(alpha(n)) where n = card (uf.dom) *)
      requires mem e1 uf.dom
      requires mem e2 uf.dom
      modifies uf
      ensures  uf.dom = old uf.dom
      ensures  uf.rep = old uf.rep
      ensures  uf.img = old uf.img
      ensures  b <-> uf.rep e1 = uf.rep e2
*)

val get : 'a elem -> 'a
(*@ v = get [uf: 'a uf] e
      (* requires $ O(alpha(n)) where n = card (uf.dom) *)
      requires mem e uf.dom
      modifies uf
      ensures  uf.dom = old uf.dom
      ensures  uf.rep = old uf.rep
      ensures  uf.img = old uf.img
      ensures  v = uf.img e
*)

val set : 'a elem -> 'a -> unit
(*@ set [uf: 'a uf] e v
      (* requires $ O(alpha(n)) where n = card (uf.dom) *)
      requires mem e uf.dom
      modifies uf
      ensures  uf.dom = old uf.dom
      ensures  uf.rep = old uf.rep
      ensures  forall x. uf.img x = if equiv uf x e then v else old (uf.img x)
*)

val union : 'a elem -> 'a elem -> unit
(*@ union [uf: 'a uf] e1 e2
      (* requires $ O(alpha(n)) where n = card (uf.dom) *)
      requires mem e1 uf.dom
      requires mem e2 uf.dom
      modifies uf
      ensures  uf.dom = old uf.dom
      ensures  exists r. (r = old (uf.rep e1) || r = old (uf.rep e2)) &&
           forall x. mem x uf.dom ->
           uf.rep x = (if old (equiv uf x e1 || equiv uf x e2) then r
                      else old (uf.rep x))
        && uf.img x = if old (equiv uf x e1 || equiv uf x e2) then uf.img r
                      else old (uf.img x)
*)

(*@ val join: 'a uf -> 'a uf -> unit *)
(*@ join uf1 uf2
    (*? requires disjoint uf1.dom uf2.dom *)
    modifies uf1
    consumes uf2
    ensures  uf1.dom = old uf1.dom `union` old uf2.dom
    ensures  forall x.
      uf1.rep x = if mem x (old uf1.dom) then old (uf1.rep x)
                                         else old (uf2.rep x)
    ensures  forall x.
      uf1.img x = if mem x (old uf1.dom) then old (uf1.img x)
                                         else old (uf2.img x)
*)
