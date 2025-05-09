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

val add_tvars : Ident.t list -> unit
(** [add_tvars l] ensures that Inferno generated type variables will not have
    names equal to those in [l]. *)

val clear_tvars : unit -> unit
(** [clear_tvars ()] ensures that the next batch of Inferno generated type
    variables will have no restrictions. *)

val ty_arrow : ty -> ty -> ty
(** [ty_arrow ty1 ty2] creates an arrow type where the argument is of type [ty1]
    and the result is [ty2]*)

val ty_prop : ty
(** Type of propositions *)

val mk_info :
  ?alias:ty option -> ?model:ty option -> Id_uast.qualid -> Id_uast.app_info

(* The following functions are used to emit errors when it is necessary to print
   a type. *)

val incompatible_types : Lexing.position * Lexing.position -> ty -> ty -> 'a
(** [incompatible_types loc ty1 ty2] compares the expected type [ty1] and the
    received [ty2] and raises an error depending on the type of mismatch between
    them. This function never terminates without an exception. *)

val cycle : Lexing.position * Lexing.position -> ty -> 'a
(** [cycle loc t] emits an error for the cyclic type [t]. *)

val ocaml_no_model : Location.t -> ty -> 'a
(** [ocaml_no_model loc id ty] emits an error stating that [id] is an OCaml
    variable of type [ty] that has no valid Gospel representation. *)

val invalid_header_unit : loc:Location.t -> ty -> 'a
(** [ocaml_no_model loc ty] emits an error stating that the header at location
    [loc] has a pattern that expected a value of type unit by received one of
    type [ty] *)
