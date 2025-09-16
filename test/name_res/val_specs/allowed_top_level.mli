(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val x : int ref
val f1 : int -> unit
(*@ f1 y
    modifies x
    requires x = 0
    ensures x = y *)

val f2 : unit -> int
(*@ y = f2 ()
    preserves x
    requires x >= 0
    ensures y = x *)

val f3 : int -> unit
(*@ f3 y
    consumes x
    requires x = 0
    ensures x = 0 *)

module M : sig
  type t
  (*@ mutable model : integer *)

  val x : t
end

val f5 : M.t -> int
(*@ _ = f5 y
    preserves x
    ensures y = y *)

val f6 : unit -> int
(*@ x = f6 ()
    preserves M.x
    ensures M.x = x *)

val f8 : unit -> bool
(*@ b = f8 ()
    preserves M.x
    preserves x
    ensures b <-> x = M.x *)
