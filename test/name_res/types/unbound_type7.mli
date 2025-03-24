(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t1 *)

(*@ type test = t2 *)

(* {gospel_expected|
[1] File "unbound_type7.mli", line 13, characters 16-18:
    13 | (*@ type test = t2 *)
                         ^^
    Error: Unbound type constructor t2
    
|gospel_expected} *)
