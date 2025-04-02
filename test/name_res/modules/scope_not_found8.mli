(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module N : sig
  module M : sig
    (*@ axiom test : N.(0 = 0) *)
  end
end

(* {gospel_expected|
[1] File "scope_not_found8.mli", line 13, characters 21-22:
    13 |     (*@ axiom test : N.(0 = 0) *)
                              ^
    Error: Unbound module N
    
|gospel_expected} *)
