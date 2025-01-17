(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Stack-safe and fast implementation of [Sequence.map]

    following antron's post
    https://discuss.ocaml.org/t/a-new-list-map-that-is-both-stack-safe-and-fast/865

    Important note: the function is applied to the elements starting from the
    end of the list, thus not in the same order as with [Sequence.map]. So if
    your functions has side-effects, this is not equivalent to [Sequence.map]
    but rather to a combination of [Sequence.rev] and [Sequence.map] as stated
    below. *)

val map : ('a -> 'b) -> 'a list -> 'b list
(*@ r = map f l
      ensures Sequence.length r.list_content = Sequence.length l.list_content
      ensures forall i. 0 <= i < Sequence.length l.list_content ->
                r.list_content[i] = f (l.list_content[i])
      equivalent "Sequence.rev (Sequence.map f (Sequence.rev l.list_content))" *)
