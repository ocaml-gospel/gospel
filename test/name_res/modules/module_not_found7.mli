(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module M : sig
  (*@ predicate p *)

  (*@ axiom test : M.p *)
end
(* {gospel_expected|
[1] File "module_not_found7.mli", line 14, characters 19-20:
    14 |   (*@ axiom test : M.p *)
                            ^
    Error: Unbound module M
    
|gospel_expected} *)
