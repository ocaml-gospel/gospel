(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Zippers for lists *)

type 'a t
(*@ model seq: 'a seq
    model idx: integer
    invariant 0 <= idx <= Seq.length seq *)

val empty : unit -> 'a t
(*@ z = empty ()
    ensures z.seq = Seq.empty
    ensures z.idx = 0 *) (* could be deduced from invariant *)

val is_empty : 'a t -> bool
(*@ b = is_empty z
    ensures b <-> z.seq = Seq.empty *)

val length : 'a t -> int
(*@ r = length z
    ensures r = Seq.length z.seq *)

val to_list : 'a t -> 'a list
(*@ l = to_list z
    ensures z.seq = l *)

val make : 'a list -> 'a t
(*@ z = make l
    ensures z.seq = l
    ensures z.idx = 0 *)

val move_left : 'a t -> 'a t
(*@ r = move_left z
    requires 0 < z.idx
    ensures  r.seq = z.seq
    ensures  r.idx = z.idx - 1 *)

val insert_left : 'a -> 'a t -> 'a t
(*@ r = insert_left x z
    ensures  r.seq = Seq.snoc z.seq[.. z.idx] x ++ z.seq[z.idx ..]
    ensures  r.idx = z.idx + 1 *)

val remove_left : 'a t -> 'a t
(*@ r = remove_left z
    requires 0 < z.idx
    ensures  r.seq = z.seq[.. z.idx - 1] ++ z.seq[z.idx ..]
    ensures  r.idx = z.idx - 1*)

val move_all_left : 'a t -> 'a t
(*@ r = move_all_left z
      ensures r.seq = z.seq
      ensures r.idx = 0 *)

val move_right : 'a t -> 'a t
(*@ r = move_right z
    requires z.idx < Seq.length z.seq
    ensures  r.seq = z.seq
    ensures  r.idx = z.idx + 1 *)

val insert_right : 'a -> 'a t -> 'a t
(*@ r = insert_right x z
    ensures  r.seq = z.seq[.. z.idx] ++ Seq.cons x z.seq[z.idx ..]
    ensures  r.idx = z.idx *)

val remove_right : 'a t -> 'a t
(*@ r = remove_right z
    requires z.idx < Seq.length z.seq
    ensures  r.seq = z.seq[.. z.idx] ++ z.seq[z.idx + 1 ..]
    ensures  r.idx = z.idx *)

val move_all_right : 'a t -> 'a t
(*@ r = move_all_right z
    ensures r.seq = z.seq
    ensures r.idx = Seq.length z.seq *)

val is_focused : 'a t -> bool
(*@ b = is_focused z
    ensures b <-> z.idx < Seq.length z.seq *)

val focused : 'a t -> 'a option
(*@ r = focused z
    ensures match r with
            | None   -> z.idx = Seq.length z.seq
            | Some x -> z.idx < Seq.length z.seq /\ x = z.seq[z.idx] *)
