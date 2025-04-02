(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax : (fun x -> x) x *)

(* {gospel_expected|
[1] File "escape_scope3.mli", line 11, characters 28-29:
    11 | (*@ axiom ax : (fun x -> x) x *)
                                     ^
    Error: Unbound value x
    
|gospel_expected} *)
