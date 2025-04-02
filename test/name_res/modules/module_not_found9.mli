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

  module N : sig
    (*@ axiom test : M.N.p *)
  end
end
(* {gospel_expected|
[1] File "module_not_found9.mli", line 15, characters 21-22:
    15 |     (*@ axiom test : M.N.p *)
                              ^
    Error: Unbound module M
    
|gospel_expected} *)
