(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type 'a structure =
  | Tyapp of Id_uast.qualid * 'a list
  | Tyarrow of 'a * 'a
  | Tytuple of 'a list
  | Tvar of Ident.t

val integer_id : Ident.t
val char_id : Ident.t
val string_id : Ident.t
val float_id : Ident.t
val prop_id : Ident.t
val set_id : Ident.t
val ty_bool : 'a structure
val ty_integer : 'a structure
val ty_char : 'a structure
val ty_string : 'a structure
val ty_float : 'a structure
val ty_prop : 'a structure
val ty_set : 'a -> 'a structure
val primitive_list : (string * Ident.t) list
val ty_arrow : 'a -> 'a -> 'a structure
val iter : ('a -> unit) -> 'a structure -> unit
val fold : ('a -> 'b -> 'b) -> 'a structure -> 'b -> 'b
val map : ('a -> 'b) -> 'a structure -> 'b structure

exception InconsistentConjunction

val conjunction :
  ('a -> 'a -> unit) -> 'a structure -> 'a structure -> 'a structure

val pprint : 'a -> 'b -> 'c
