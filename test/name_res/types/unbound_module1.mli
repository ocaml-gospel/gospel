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
  (*@ type t *)
end

(*@ type test = t *)

(* {gospel_expected|
[1] File "unbound_module1.mli", line 15, characters 16-17:
    15 | (*@ type test = t *)
                         ^
    Error: Unbound type constructor t
    
|gospel_expected} *)
