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
(*@ mutable *)

val free : t -> unit
(*@ free x
    consumes x
    ensures x = x
*)

(* {gospel_expected|
[1] File "not_produced.mli", line 17, characters 16-17:
    17 |     ensures x = x
                         ^
    Error: Unbound value x
    
|gospel_expected} *)
