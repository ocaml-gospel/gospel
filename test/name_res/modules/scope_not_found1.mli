(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : M.(0 = 0) *)

(* {gospel_expected|
[1] File "scope_not_found1.mli", line 11, characters 17-18:
    11 | (*@ axiom test : M.(0 = 0) *)
                          ^
    Error: Unbound module M
    
|gospel_expected} *)
