(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t

val f : unit -> t
(*@ let x = f () in
      ensures x = x *)

(* {gospel_expected|
[1] File "no_gospel_rep1.mli", line 15, characters 18-19:
    15 |       ensures x = x *)
                           ^
    Error: Unbound value x
    
|gospel_expected} *)
