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

(*@ type t *)

(*@ type test = M.t *)

(* {gospel_expected|
[1] File "unbound_module3.mli", line 15, characters 18-19:
    15 | (*@ type test = M.t *)
                           ^
    Error: Unbound type constructor M.t
    
|gospel_expected} *)
