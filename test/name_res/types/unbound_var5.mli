(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type ('c, 'b) t = ('b, 'a) map *)

(* {gospel_expected|
[1] File "unbound_var5.mli", line 11, characters 27-29:
    11 | (*@ type ('c, 'b) t = ('b, 'a) map *)
                                    ^^
    Error: The type variable 'a is unbound in this type declaration
    
|gospel_expected} *)
