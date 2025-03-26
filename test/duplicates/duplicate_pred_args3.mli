(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate f (x : integer) (y : integer) (x : integer) *)

(* {gospel_expected|
[1] File "duplicate_pred_args3.mli", line 11, characters 45-46:
    11 | (*@ predicate f (x : integer) (y : integer) (x : integer) *)
                                                      ^
    Error: Duplicated argument x
    
|gospel_expected} *)
