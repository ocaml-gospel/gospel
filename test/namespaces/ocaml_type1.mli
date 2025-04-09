(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t1

(*@ type t2 = t1 *)

(* {gospel_expected|
[1] File "ocaml_type1.mli", line 13, characters 14-16:
    13 | (*@ type t2 = t1 *)
                       ^^
    Error: Unbound type constructor t1
    
|gospel_expected} *)
