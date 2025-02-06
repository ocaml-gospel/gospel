open Identifier
open Ttypes

type vsymbol = { vs_name : Ident.t; vs_ty : ty }

type lsymbol =
  | Function_symbol of { ls_name : Ident.t; ls_args : ty list; ls_value : ty }
  | Constructor_symbol of {
      ls_name : Ident.t;
      ls_args : constructor_arguments;
      ls_value : ty;
    }
  | Field_symbol of { ls_name : Ident.t; ls_args : ty list; ls_value : ty }

and constructor_arguments =
  | Cstr_tuple of ty list
  | Cstr_record of lsymbol list

module Svs : Set.S with type elt = vsymbol
module Mvs : Map.S with type key = vsymbol
module Sls : Set.S with type elt = lsymbol
module Mls : Map.S with type key = lsymbol

val ls_equal : lsymbol -> lsymbol -> bool
val create_vsymbol : ?path:string list -> Preid.t -> ty -> vsymbol
val function_symbol : Ident.t -> ty list -> ty -> lsymbol
val constructor_symbol : Ident.t -> constructor_arguments -> ty -> lsymbol
val field_symbol : Ident.t -> ty list -> ty -> lsymbol
val get_name : lsymbol -> Ident.t
val get_value : lsymbol -> ty
val get_args : lsymbol -> ty list
val is_fs_tuple : lsymbol -> bool

val ls_subst_ts : tysymbol -> tysymbol -> lsymbol -> lsymbol
(** [ls_subst_ts old_ts new_ts symbol] is the substitution of [new_ts] for
    [old_ts] in every [ty]s of [symbol]. *)

val ls_subst_ty : tysymbol -> tysymbol -> ty -> lsymbol -> lsymbol
(** [ls_subst_ty old_ts new_ts new_ty symbol] is the substitution of an
    instanciation of [new_ts] with [new_ty] for [old_ts] in every [ty]s of
    [symbol]. *)

val fs_unit : lsymbol
val fs_bool_true : lsymbol
val fs_bool_false : lsymbol
val fs_option_none : lsymbol
val fs_option_some : lsymbol
val fs_list_nil : lsymbol
val fs_list_cons : lsymbol
val fs_tuple : int -> lsymbol
val fs_apply : lsymbol
val ps_equ : lsymbol
val ps_nequ : lsymbol
val ps_not : lsymbol
val pp_vsymbol : vsymbol Fmt.t
val pp_lsymbol : lsymbol Fmt.t
