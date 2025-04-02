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
  module M : sig end
end

(*@ axiom test : M.(0 = 0) *)

(* {gospel_expected|
[1] File "scope_not_found3.mli", line 15, characters 17-18:
    15 | (*@ axiom test : M.(0 = 0) *)
                          ^
    Error: Unbound module M
    
|gospel_expected} *)
