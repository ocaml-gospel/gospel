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
  (*@ predicate f *)
end

(*@ axiom test : f *)

(* {gospel_expected|
[1] File "unbound_module4.mli", line 15, characters 17-18:
    15 | (*@ axiom test : f *)
                          ^
    Error: Unbound value f
    
|gospel_expected} *)
