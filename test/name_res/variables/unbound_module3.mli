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
  (*@ predicate g *)
end

(*@ predicate f *)

(*@ axiom test : M.f *)

(* {gospel_expected|
[1] File "unbound_module3.mli", line 17, characters 19-20:
    17 | (*@ axiom test : M.f *)
                            ^
    Error: Unbound value M.f
    
|gospel_expected} *)
