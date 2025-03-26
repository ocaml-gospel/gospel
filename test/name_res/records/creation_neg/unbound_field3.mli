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

(*@ axiom ax : forall r. { x = 0 } *)

(* {gospel_expected|
[1] File "unbound_field3.mli", line 15, characters 25-34:
    15 | (*@ axiom ax : forall r. { x = 0 } *)
                                  ^^^^^^^^^
    Error: No record found with the provided labels
    
|gospel_expected} *)
