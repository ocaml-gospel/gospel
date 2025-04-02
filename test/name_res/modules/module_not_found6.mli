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

(*@ open N *)
(*@ open N.M *)

(* {gospel_expected|
[1] File "module_not_found6.mli", line 14, characters 11-12:
    14 | (*@ open N.M *)
                    ^
    Error: Unbound module M
    
|gospel_expected} *)
