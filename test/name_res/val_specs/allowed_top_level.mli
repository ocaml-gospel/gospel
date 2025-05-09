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
(*@ modifies x
    requires x = 0
    let () = f1 y in
      ensures x = y *)

val f2 : unit -> int
(*@ preserves x
    requires x >= 0
    let y = f2 () in
      ensures y = x *)

val f3 : int -> unit
(*@ consumes x
    requires x = 0
      let () = f3 y in
      ensures x = 0 *)

module M : sig
  type t
  (*@ mutable model : integer *)

  val x : t
end

val f5 : M.t -> int
(*@ preserves x
    let _ = f5 y in
      ensures y = y *)

val f6 : unit -> int
(*@ preserves M.x
    let x = f6 () in
      ensures M.x = x *)

val f8 : unit -> bool
(*@ preserves M.x
    preserves x
    let b = f8 () in
      ensures b <-> x = M.x *)
