(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ppxlib
open Ttypes
open Tterm
module Ident = Identifier.Ident

type lb_arg =
  | Lunit  (** () *)
  | Lnone of vsymbol  (** x *)
  | Loptional of vsymbol  (** ?x *)
  | Lnamed of vsymbol  (** ~x *)
  | Lghost of vsymbol  (** \[x: t\] *)

type val_spec = {
  sp_args : lb_arg list;  (** Arguments *)
  sp_ret : lb_arg list;
      (** Return values. This is a list because of tuple destruction. *)
  sp_pre : term list;  (** Preconditions *)
  sp_checks : term list;  (** Checks preconditions *)
  sp_post : term list;  (** Postconditions *)
  sp_xpost : (xsymbol * (pattern * term) list) list;
      (** Exceptional postconditions. *)
  sp_wr : term list;  (** Writes *)
  sp_cs : term list;  (** Consumes *)
  sp_diverge : bool;  (** Diverges *)
  sp_pure : bool;  (** Pure *)
  sp_equiv : string list;  (** Equivalent *)
  sp_text : string;
      (** String containing the original specificaion as written by the user *)
  sp_loc : Location.t;  (** Specification location *)
}

type val_description = {
  vd_name : Ident.t;
  vd_type : Parsetree.core_type;
  vd_prim : string list;  (** primitive declaration *)
  vd_attrs : Parsetree.attributes;
  vd_args : lb_arg list;
  vd_ret : lb_arg list;
  vd_spec : val_spec option;
  vd_loc : Location.t;
}

type type_spec = {
  ty_ephemeral : bool;  (** Ephemeral *)
  ty_fields : (lsymbol * bool) list;  (** Models (field symbol * mutable) *)
  ty_invariants : term list;  (** Invariants *)
  ty_text : string;
      (** String containing the original specificaion as written by the user *)
  ty_loc : Location.t;  (** Specification location *)
}

type mutable_flag = Immutable | Mutable

(* used for both record declarations and variant declarations *)
type 'a label_declaration = {
  ld_field : 'a;
  ld_mut : mutable_flag;
  ld_loc : Location.t;
  ld_attrs : Parsetree.attributes; (* l : T [@id1] [@id2] *)
}

type rec_declaration = {
  rd_cs : lsymbol;
  rd_ldl : lsymbol label_declaration list;
}

type constructor_decl = {
  cd_cs : lsymbol;
  (* constructor *)
  (* cd_ld is empty if defined through a tuple *)
  cd_ld : (Ident.t * ty) label_declaration list;
  cd_loc : Location.t;
  cd_attrs : Parsetree.attributes; (* C of ... [@id1] [@id2] *)
}

type type_kind =
  | Pty_abstract
  | Pty_variant of constructor_decl list
  | Pty_record of rec_declaration
  (* Invariant: non-empty list *)
  | Pty_open

type private_flag = Private | Public

type type_declaration = {
  td_ts : tysymbol;
  td_params : (tvsymbol * (variance * injectivity)) list;
  (* the core_type in uast can only be Ptyp_var _ or Ptyp_any
     according to the parser *)
  td_cstrs : (ty * ty * Location.t) list;
  td_kind : type_kind;
  td_private : private_flag;
  td_manifest : ty option;
  td_attrs : Parsetree.attributes;
  td_spec : type_spec option;
  td_loc : Location.t;
}

type axiom = {
  ax_name : Ident.t;  (** Name *)
  ax_term : term;  (** Definition *)
  ax_loc : Location.t;  (** Location *)
  ax_text : string;
      (** String containing the original specificaion as written by the user *)
}

type fun_spec = {
  fun_req : term list;  (** Preconditions *)
  fun_ens : term list;  (** Postconditions *)
  fun_variant : term list;  (** Variant *)
  fun_coer : bool;  (** Coercion *)
  fun_text : string;
      (** String containing the original specificaion as written by the user *)
  fun_loc : Location.t;  (** Specification location *)
}

type function_ = {
  fun_ls : lsymbol;  (** Function symbol *)
  fun_rec : bool;  (** Recursive *)
  fun_params : vsymbol list;  (** Arguments *)
  fun_def : term option;  (** Definition *)
  fun_spec : fun_spec option;  (** Specification *)
  fun_text : string;
      (** String containing the original specificaion as written by the user *)
  fun_loc : Location.t;  (** Location *)
}

type extension_constructor = {
  ext_ident : Ident.t;
  ext_xs : xsymbol;
  ext_kind : Parsetree.extension_constructor_kind;
  ext_loc : Location.t;
  ext_attributes : Parsetree.attributes; (* C of ... [@id1] [@id2] *)
}

type type_exception = {
  exn_constructor : extension_constructor;
  exn_loc : Location.t;
  exn_attributes : Parsetree.attributes; (* ... [@@id1] [@@id2] *)
}

type rec_flag = Nonrecursive | Recursive
type ghost = bool

type with_constraint =
  | Wty of Ident.t * type_declaration
  (* with type X.t = ...

     Note: the last component of the longIdent.t must match
     the name of the type_declaration. *)
  | Wmod of Ident.t * Ident.t
  (* with module X.Y = Z *)
  | Wtysubs of Ident.t * type_declaration
  (* with type X.t := ..., same format as [Pwith_type] *)
  | Wmodsubs of Ident.t * Ident.t
(* with module X.Y := Z *)

type open_description = {
  opn_id : string list;
  opn_override : Asttypes.override_flag;
  opn_loc : Location.t;
  opn_attrs : Parsetree.attributes;
}

type signature = signature_item list

and signature_item = { sig_desc : signature_item_desc; sig_loc : Location.t }

and signature_item_desc =
  | Sig_val of val_description * ghost
  | Sig_type of rec_flag * type_declaration list * ghost
  (* type t1 = ... and ... and tn = ... *)
  | Sig_typext of Parsetree.type_extension
  (* type t1 += ... *)
  | Sig_module of module_declaration
  (* module X : MT *)
  | Sig_recmodule of module_declaration list
  (* module rec X1 : MT1 and ... and Xn : MTn *)
  | Sig_modtype of module_type_declaration
  (* module type S = MT
     module type S *)
  (* these were not modified *)
  | Sig_exception of type_exception
  (* exception C of T *)
  | Sig_open of open_description * ghost
  (* open X *)
  | Sig_include of Parsetree.include_description
  (* include MT *)
  | Sig_class of Parsetree.class_description list
  (* class c1 : ... and ... and cn : ... *)
  | Sig_class_type of Parsetree.class_type_declaration list
  (* class type ct1 = ... and ... and ctn = ... *)
  | Sig_attribute of Parsetree.attribute
  (* [@@@id] *)
  | Sig_extension of Parsetree.extension * Parsetree.attributes
  (* [%%id] *)
  (* Specific to specification *)
  | Sig_use of string
  | Sig_function of function_
  | Sig_axiom of axiom

and module_declaration = {
  md_name : Ident.t;
  md_type : module_type;
  md_attrs : Parsetree.attributes;
  (* ... [@@id1] [@@id2] *)
  md_loc : Location.t;
}

and module_type_declaration = {
  mtd_name : Ident.t;
  mtd_type : module_type option;
  mtd_attrs : Parsetree.attributes;
  (* ... [@@id1] [@@id2] *)
  mtd_loc : Location.t;
}

and module_type = {
  mt_desc : module_type_desc;
  mt_loc : Location.t;
  mt_attrs : Parsetree.attributes; (* ... [@id1] [@id2] *)
}

and module_type_desc =
  | Mod_ident of string list
  (* S *)
  | Mod_signature of signature
  (* sig ... end *)
  | Mod_functor of Ident.t * module_type option * module_type
  (* functor(X : MT1) -> MT2 *)
  | Mod_with of module_type * with_constraint list
  (* MT with ... *)
  | Mod_typeof of Parsetree.module_expr
  (* module type of ME *)
  | Mod_extension of Parsetree.extension
  (* [%id] *)
  | Mod_alias of string list
(* (module M) *)
