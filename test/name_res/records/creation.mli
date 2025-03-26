(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Positive tests for creation of records *)

(*@ type t1 = { x : integer } *)

(*@ type t2 = { y : integer } *)

(*@ function v1 : t1 = { x = 0 } *)
(*@ function v2 : t2 = { y = 0 } *)

module M1 : sig
  (*@ type t3 = { x : integer; y : integer } *)

  (*@ function v1 : t1 = { x = 0 } *)
  (*@ function v2 : t2 = { y = 0 } *)
  (*@ function v3 : t3 = { x = 0; y = 0 } *)
end

module M2 : sig
  (*@ type t3 = { x : integer; y : integer } *)
end

(*@ type t3 = { w : integer; z : integer } *)

(*@ function v3 : M1.t3 = { M1.x = 0; y = 0 } *)
(*@ function v4 : M1.t3 = { M1.x = 0; M1.y = 0 } *)
(*@ function v5 : M1.t3 = { x = 0; M1.y = 0 } *)
(*@ function v6 : M2.t3 = { M2.x = 0; y = 0 } *)
(*@ function v7 : M2.t3 = { M2.x = 0; M2.y = 0 } *)
(*@ function v8 : M2.t3 = { x = 0; M2.y = 0 } *)
(*@ function v9 : t3 = { w = 0; z = 0 } *)
