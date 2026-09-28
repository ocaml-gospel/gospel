(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type t1 = [ `A | `B ]
type t2 = { x : t1 }
(* {gospel_expected|
[1] File "./unsupported_types.mli", line 12, characters 16-18:
    12 | type t2 = { x : t1 }
                         ^^
    Error: Not yet supported: t1
    
|gospel_expected} *)
