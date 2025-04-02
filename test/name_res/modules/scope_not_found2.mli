(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module N : sig end

(*@ axiom test : M.(0 = 0) *)

(* {gospel_expected|
[1] File "scope_not_found2.mli", line 13, characters 17-18:
    13 | (*@ axiom test : M.(0 = 0) *)
                          ^
    Error: Unbound module M
    
|gospel_expected} *)
