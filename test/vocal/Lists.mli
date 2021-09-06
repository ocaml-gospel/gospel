(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Stack-safe and fast implementation of [List.map]

    following antron's post
    https://discuss.ocaml.org/t/a-new-list-map-that-is-both-stack-safe-and-fast/865

    Important note: the function is applied to the elements starting from the
    end of the list, thus not in the same order as with [List.map]. So if your
    functions has side-effects, this is not equivalent to [List.map] but
    rather to a combination of [List.rev] and [List.map] as stated below.
 *)

val map: ('a -> 'b) -> 'a list -> 'b list
(*@ r = map f l
      ensures length r = length l
      ensures forall i. 0 <= i < length l -> r[i] = f l[i]
      equivalent "List.rev (List.map f (List.rev l))" *)
