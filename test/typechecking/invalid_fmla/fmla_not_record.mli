(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = { x : integer } *)

(*@ axiom test : { x = 0 } *)

(* {gospel_expected|
[1] File "fmla_not_record.mli", line 13, characters 17-26:
    13 | (*@ axiom test : { x = 0 } *)
                          ^^^^^^^^^
    Error: Mismatch between type prop and type t
    
|gospel_expected} *)
