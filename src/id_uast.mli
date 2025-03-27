(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(* Types *)
type id = Ident.t

open Ppxlib

type qualid = Qid of id | Qdot of qualid * id

type pty =
  | PTtyvar of id
  | PTtyapp of qualid * pty list
  | PTtuple of pty list
  | PTarrow of pty * pty

and labelled_arg =
  | Lunit
  | Lnone of id
  | Loptional of id
  | Lnamed of id
  | Lghost of id * pty

(* Logical terms and formulas *)

type binder = id * pty option
type param = id * pty

type term = { term_desc : term_desc; term_loc : Location.t }

and term_desc =
  | Ttrue
  | Tfalse
  | Tconst of constant
  | Tvar of qualid
  | Tfield of term * qualid
  | Tapply of term * term
  | Tif of term * term * term
  | Tquant of Parse_uast.quant * binder list * term
  | Tlambda of pattern list * term * pty option
  | Tattr of string * term
  | Tlet of id * term * term
  | Tcase of term * (pattern * term option * term) list
  | Tcast of term * pty
  | Ttuple of term list
  | Trecord of (qualid * term) list
  | Tscope of qualid * term
  | Told of term

(* Specification *)

type xpost = Location.t * (qualid * (pattern * term) option) list

type spec_header = {
  sp_hd_nm : id;
  (* header name *)
  sp_hd_ret : labelled_arg list;
  (* Can only be LNone or LGhost *)
  sp_hd_args : labelled_arg list; (* header arguments' names *)
}

type val_spec = {
  sp_header : spec_header option;
  sp_pre : term list;
  sp_checks : term list;
  sp_post : term list;
  sp_xpost : xpost list;
  sp_writes : term list;
  sp_consumes : term list;
  sp_diverge : bool;
  sp_pure : bool;
  sp_equiv : string list;
  sp_text : string;
  sp_loc : Location.t;
}

type field = { f_loc : Location.t; f_preid : id; f_pty : pty; f_mutable : bool }

type type_spec = {
  ty_ephemeral : bool;
  ty_field : field list;
  ty_invariant : (id * term list) option;
  ty_text : string;
  ty_loc : Location.t;
}

type fun_spec = {
  fun_req : term list;
  fun_ens : term list;
  fun_variant : term list;
  fun_coer : bool;
  fun_text : string;
  fun_loc : Location.t;
}

(* type param  = Location.t * id * pty *)
type function_ = {
  fun_name : id;
  fun_rec : bool;
  fun_type : pty option;
  fun_params : param list;
  fun_def : term option;
  fun_spec : fun_spec option;
  fun_loc : Location.t;
  fun_text : string;
}

type axiom = {
  ax_name : id;
  ax_term : term;
  ax_loc : Location.t;
  ax_text : string;
}

(* Modified OCaml constructs with specification attached *)

type s_val_description = {
  vname : string loc;
  vtype : core_type;
  vprim : string list;
  vattributes : attributes;
  (* ... [@@id1] [@@id2] *)
  vspec : val_spec option;
  (* specification *)
  vloc : Location.t;
}

type s_type_declaration = {
  tname : string loc;
  tparams : (core_type * (variance * injectivity)) list;
  (* ('a1,...'an) t; None represents  _*)
  tcstrs : (core_type * core_type * Location.t) list;
  (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
  tkind : type_kind;
  tprivate : private_flag;
  (* = private ... *)
  tmanifest : core_type option;
  (* = T *)
  tattributes : attributes;
  (* ... [@@id1] [@@id2] *)
  tspec : type_spec option;
  (* specification *)
  tloc : Location.t;
}

type s_with_constraint =
  | Wtype of Longident.t loc * s_type_declaration
  (* with type X.t = ...

     Note: the last component of the longident must match
     the name of the type_declaration. *)
  | Wmodule of Longident.t loc * Longident.t loc
  (* with module X.Y = Z *)
  | Wtypesubst of Longident.t loc * s_type_declaration
  (* with type X.t := ..., same format as [Pwith_type] *)
  | Wmodtypesubst of longident_loc * module_type
  (* with module type X.Y := sig end *)
  | Wmodtype of longident_loc * module_type
  (* with module type X.Y = Z *)
  | Wmodsubst of Longident.t loc * Longident.t loc
(* with module X.Y := Z *)

type s_signature_item_desc =
  | Sig_val of s_val_description
  (*
          val x: T
          external x: T = "s1" ... "sn"
         *)
  | Sig_type of rec_flag * s_type_declaration list
  (* type t1 = ... and ... and tn = ... *)
  | Sig_typesubst of s_type_declaration list
  (* type t1 := ... and ... and tn := ...  *)
  | Sig_typext of type_extension
  (* type t1 += ... *)
  | Sig_module of s_module_declaration
  (* module X : MT *)
  | Sig_recmodule of s_module_declaration list
  (* module rec X1 : MT1 and ... and Xn : MTn *)
  | Sig_modtype of s_module_type_declaration
  (* module type S = MT
     module type S *)
  | Sig_modtypesubst of s_module_type_declaration
  (* module type S :=  ...  *)
  (* these were not modified *)
  | Sig_modsubst of module_substitution
  (* module X := M *)
  | Sig_exception of type_exception
  (* exception C of T *)
  | Sig_open of open_description
  (* open X *)
  | Sig_include of include_description
  (* include MT *)
  | Sig_class of class_description list
  (* class c1 : ... and ... and cn : ... *)
  | Sig_class_type of class_type_declaration list
  (* class type ct1 = ... and ... and ctn = ... *)
  | Sig_attribute of attribute
  (* [@@@id] *)
  | Sig_extension of extension * attributes
  (* [%%id] *)
  (* Specific to specification *)
  | Sig_function of function_
  | Sig_axiom of axiom
  | Sig_ghost_type of rec_flag * s_type_declaration list
  | Sig_ghost_val of s_val_description
  | Sig_ghost_open of open_description

and s_signature_item = { sdesc : s_signature_item_desc; sloc : Location.t }
and s_signature = s_signature_item list

and s_module_type_desc =
  | Mod_ident of Longident.t loc
  (* S *)
  | Mod_signature of s_signature
  (* sig ... end *)
  | Mod_functor of s_functor_parameter * s_module_type
  (* functor(X : MT1) -> MT2 *)
  | Mod_with of s_module_type * s_with_constraint list
  (* MT with ... *)
  | Mod_typeof of module_expr
  (* module type of ME *)
  | Mod_extension of extension
  (* [%id] *)
  | Mod_alias of Longident.t loc
(* (module M) *)

and s_functor_parameter =
  | Unit
  (* () *)
  | Named of string option loc * s_module_type
(* (X : MT)          Some X, MT
   (_ : MT)          None, MT *)

and s_module_type = {
  mdesc : s_module_type_desc;
  mloc : Location.t;
  mattributes : attributes; (* ... [@id1] [@id2] *)
}

and s_module_declaration = {
  mdname : id option;
  mdtype : s_module_type;
  mdattributes : attributes;
  (* ... [@@id1] [@@id2] *)
  mdloc : Location.t;
}

and s_module_type_declaration = {
  mtdname : string loc;
  mtdtype : s_module_type option;
  mtdattributes : attributes;
  (* ... [@@id1] [@@id2] *)
  mtdloc : Location.t;
}
