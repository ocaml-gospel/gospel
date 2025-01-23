open Identifier

type tvsymbol = { tv_name : Ident.t }

type ty = { ty_node : ty_node }
and ty_node = Tyvar of tvsymbol | Tyapp of tysymbol * ty list

and tysymbol = {
  ts_ident : Ident.t;
  ts_args : tvsymbol list;
  (* we need to keep variables to do things like
     type ('a,'b) t1  type ('a,'b) t2 = ('b,'a) t1 *)
  ts_alias : ty option;
}

type exn_type =
  | Exn_tuple of ty list
  (* exception E of int * int
       -> Exn_tuple [int_ty;int_ty]
     exception E of (int*int)
       -> Exn_tuple [Tyapp (ts_tuple 2) [ty_int;ty_int]] *)
  | Exn_record of (Ident.t * ty) list

type xsymbol = { xs_ident : Ident.t; xs_type : exn_type }

val ts_ident : tysymbol -> Ident.t
val ts_args : tysymbol -> tvsymbol list
val tv_equal : tvsymbol -> tvsymbol -> bool
val ty_equal : ty -> ty -> bool
val ts_equal : tysymbol -> tysymbol -> bool
val xs_equal : xsymbol -> xsymbol -> bool

module Htv : Hashtbl.S with type key = tvsymbol
module Mtv : Map.S with type key = tvsymbol
module Hts : Hashtbl.S with type key = tysymbol
module Mts : Map.S with type key = tysymbol
module Mxs : Map.S with type key = xsymbol

val ts_subst_ts : tysymbol -> tysymbol -> tysymbol -> tysymbol
(** [ts_subst_ts old_ts new_ts ts] is the substitution of [new_ts] for [old_ts]
    in [ts] and its type alias if any. *)

val ty_subst_ty : tysymbol -> tysymbol -> ty -> ty -> ty
(** [ty_subst_ty old_ts new_ts new_ty ty] is the substitution of the
    instanciation of the arguments of [new_ts] with the ones of [new_ty] for
    [old_ts] in [ty]. *)

val ts_subst_ty : tysymbol -> tysymbol -> ty -> tysymbol -> tysymbol
(** [ts_subst_ty old_ts new_ts new_ty ts] runs
    [ty_subst_ty old_ts new_ts new_ty] on the type alias if any. *)

val ty_subst_ts : tysymbol -> tysymbol -> ty -> ty
(** [ty_subst_ts old_ts new_ts ty] is the substitution of [new_ts] for [old_ts]
    in [ty]. *)

val xs_subst_ts : tysymbol -> tysymbol -> xsymbol -> xsymbol
val xs_subst_ty : tysymbol -> tysymbol -> ty -> xsymbol -> xsymbol
val create_tv : Ident.t -> tvsymbol
val tv_of_string : ?loc:Ppxlib.Location.t -> string -> tvsymbol
val fresh_ty_var : ?loc:Ppxlib.Location.t -> string -> ty
val xsymbol : Ident.t -> exn_type -> xsymbol
val ty_of_var : tvsymbol -> ty
val ty_match : ty Mtv.t -> ty -> ty -> ty Mtv.t
val ts_match_args : ?loc:Ppxlib.Location.t -> tysymbol -> 'a list -> 'a Mtv.t
val ty_app : ?loc:Ppxlib.Location.t -> tysymbol -> ty list -> ty
val ty_full_inst : ?loc:Ppxlib.Location.t -> ty Mtv.t -> ty -> ty
val mk_ts : Ident.t -> tvsymbol list -> ty option -> tysymbol
val ts_arity : tysymbol -> int
val is_ts_tuple : tysymbol -> bool
val ts_arrow : tysymbol
val ts_unit : tysymbol
val ts_exn : tysymbol
val ts_bool : tysymbol
val ts_char : tysymbol
val ts_bytes : tysymbol
val ts_string : tysymbol
val ts_int32 : tysymbol
val ts_int64 : tysymbol
val ts_nativeint : tysymbol
val ts_int : tysymbol
val ts_integer : tysymbol
val ts_float : tysymbol
val ts_option : tysymbol
val ts_list : tysymbol
val ts_array : tysymbol
val ts_tuple : int -> tysymbol
val ts_format6 : tysymbol
val ts_lazy : tysymbol
val ty_unit : ty
val ty_bool : ty
val ty_char : ty
val ty_int : ty
val ty_string : ty
val ty_integer : ty
val ty_float : ty
val ty_option : ty -> ty
val ty_list : ty -> ty
val ty_tuple : ty list -> ty
val pp_ty : ty Fmt.t
val pp_tvsymbol : tvsymbol Fmt.t
val pp_tysymbol : tysymbol Fmt.t
val pp_xsymbol : xsymbol Fmt.t
val print_ty : ty Fmt.t
val print_ts : tysymbol Fmt.t
val print_tv : tvsymbol Fmt.t
val print_xs : xsymbol Fmt.t
