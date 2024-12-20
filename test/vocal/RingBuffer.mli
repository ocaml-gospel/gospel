(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ open Sequence *)

type 'a buffer
(*@ mutable model sequence: 'a sequence
            model capacity: integer
    with self
    invariant length self.sequence <= self.capacity <= Sys.max_array_length.v *)

val create : int -> 'a -> 'a buffer
(*@ b = create n dummy
      requires 0 < n.v <= Sys.max_array_length.v
      ensures  b.capacity = n.v
      ensures  b.sequence = empty *)

val length : 'a buffer -> int
(*@ n = length b
      ensures n.v = length b.sequence *)

val clear : 'a buffer -> unit
(*@ clear b
      modifies b
      ensures  b.sequence = empty *)

val push : 'a buffer -> 'a -> unit
(*@ push b elt
      requires length b.sequence < b.capacity
      modifies b
      ensures  length b.sequence = length (old b.sequence) + 1
      ensures  b.sequence = old b.sequence ++ (cons elt empty) *)

val peek : 'a buffer -> 'a
(*@ r = peek b
      requires length b.sequence > 0
      ensures  r = b.sequence[0] *)

val pop : 'a buffer -> 'a
(*@ r = pop b
      requires length b.sequence > 0
      modifies b
      ensures  length b.sequence = length (old b.sequence) - 1
      ensures  r = (old b.sequence)[0]
      ensures  old b.sequence = cons r b.sequence *)

val get : 'a buffer -> int -> 'a
(*@ r = get b i
      requires 0 <= i.v < length b.sequence
      ensures  r = b.sequence[i.v] *)

val copy : 'a buffer -> 'a buffer
(*@ r = copy b
      ensures length b.sequence = length r.sequence
      ensures b.capacity = r.capacity
      ensures forall i. 0 <= i < length r.sequence ->
        b.sequence[i] = r.sequence[i] *)
