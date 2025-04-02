(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module M : sig end

(*@ axiom ax : M *)

(* {gospel_expected|
[1] File "var_not_module.mli", line 13, characters 15-16:
    13 | (*@ axiom ax : M *)
                        ^
    Error: Unbound value M
    
|gospel_expected} *)
