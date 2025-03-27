(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(* Definitions necessary for Inferno. Should not be used directly *)
type ty = Id_uast.pty
type tyvar
type 'a structure = 'a Structure.structure

val inject : int -> tyvar
val pprint : 'a -> 'b -> 'c
val variable : tyvar -> ty
val structure : ty structure -> ty
val mu : tyvar -> ty -> ty

(*  End of Inferno definitions *)

val ty_arrow : ty -> ty -> ty
(** [ty_arrow ty1 ty2] creates an arrow type where the argument is of type [ty1]
    and the result is [ty2]*)

val ty_prop : ty
(** Type of propositions *)

val mk_info : ?alias:ty option -> Id_uast.qualid -> Id_uast.app_info

val incompatible_types : Lexing.position * Lexing.position -> ty -> ty -> 'a
(** [incompatible_types loc ty1 ty2] compares the expected type [ty1] and the
    received [ty2] and raises an error depending on the type of mismatch between
    them. This function never terminates without an exception. *)

val cycle : Lexing.position * Lexing.position -> ty -> 'a
(** [cycle loc t] emits an error for the cyclic type [t]. *)
