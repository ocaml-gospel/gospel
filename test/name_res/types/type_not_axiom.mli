(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax : True *)

(*@ type t = ax *)

(* {gospel_expected|
[1] File "type_not_axiom.mli", line 13, characters 13-15:
    13 | (*@ type t = ax *)
                      ^^
    Error: Unbound type constructor ax
    
|gospel_expected} *)
