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

(*@ type test = M.t *)

(* {gospel_expected|
[1] File "unbound_module2.mli", line 13, characters 18-19:
    13 | (*@ type test = M.t *)
                           ^
    Error: Unbound type constructor M.t
    
|gospel_expected} *)
