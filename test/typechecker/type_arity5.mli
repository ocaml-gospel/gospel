(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type 'a t1
type t2 = t1

(* ERROR:
   Line 13
   wrong number of arguments
   one argument must be given to t1 *)

(* {gospel_expected|
   [125] File "type_arity5.mli", line 12, characters 10-12:
         12 | type t2 = t1
                        ^^
         Error: The type t1 expects 1 argument(s) but was given 0 argument(s) here.
   |gospel_expected} *)
