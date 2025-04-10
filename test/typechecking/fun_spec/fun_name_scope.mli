(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate rec f
      requires f *)
(* {gospel_expected|
[1] File "fun_name_scope.mli", line 12, characters 15-16:
    12 |       requires f *)
                        ^
    Error: Unbound value f
    
|gospel_expected} *)
