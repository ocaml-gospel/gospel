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
  (*@ type t = { x : integer } *)
end

(*@ predicate test (x : M.t) = x.x + x.x *)

(* {gospel_expected|
[1] File "unbound_module1.mli", line 15, characters 39-40:
    15 | (*@ predicate test (x : M.t) = x.x + x.x *)
                                                ^
    Error: Unbound record label x
    
|gospel_expected} *)
