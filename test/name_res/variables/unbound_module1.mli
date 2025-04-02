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

(*@ axiom test : M.f *)

(* {gospel_expected|
[1] File "unbound_module1.mli", line 13, characters 19-20:
    13 | (*@ axiom test : M.f *)
                            ^
    Error: Unbound value M.f
    
|gospel_expected} *)
