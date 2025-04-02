(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t *)

(*@ axiom test : t *)

(* {gospel_expected|
[1] File "var_not_type.mli", line 13, characters 17-18:
    13 | (*@ axiom test : t *)
                          ^
    Error: Unbound value t
    
|gospel_expected} *)
