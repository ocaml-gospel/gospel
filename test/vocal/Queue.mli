(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type 'a t
(*@ mutable model view: 'a sequence *)

val create : unit -> 'a t
(** Return a new queue, initially empty. *)
(*@ q = create ()
      ensures q.view = Sequence.empty *)

val push : 'a -> 'a t -> unit
(** [add x q] adds the element [x] at the end of the queue [q]. *)
(*@ push x q
      modifies q
      ensures  q.view = Sequence.snoc (old q.view) x *)

val pop : 'a t -> 'a
(** [pop q] removes and returns the first element in queue [q]. *)
(*@ r = pop q
      requires q.view <> Sequence.empty
      modifies q
      ensures  old q.view = Sequence.cons r q.view *)

val is_empty : 'a t -> bool
(** Return [true] if the given queue is empty, [false] otherwise. *)
(*@ b = is_empty q
      ensures b <-> q.view = Sequence.empty *)

val transfer : 'a t -> 'a t -> unit
(** [transfer q1 q2] adds all of [q1]'s elements at the end of the queue [q2],
    then clears [q1]. *)
(*@ transfer q1 q2
      modifies q1.view, q2.view
      ensures  q1.view = Sequence.empty
      ensures  q2.view = old q2.view ++ old q1.view *)
