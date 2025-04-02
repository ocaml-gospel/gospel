(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax : let f = (fun x -> x) in f x *)

(* {gospel_expected|
[1] File "escape_scope4.mli", line 11, characters 41-42:
    11 | (*@ axiom ax : let f = (fun x -> x) in f x *)
                                                  ^
    Error: Unbound value x
    
|gospel_expected} *)
