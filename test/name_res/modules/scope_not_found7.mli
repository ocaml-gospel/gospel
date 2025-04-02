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
  (*@ axiom test : N.(0 = 0) *)
end

(* {gospel_expected|
[1] File "scope_not_found7.mli", line 12, characters 19-20:
    12 |   (*@ axiom test : N.(0 = 0) *)
                            ^
    Error: Unbound module N
    
|gospel_expected} *)
