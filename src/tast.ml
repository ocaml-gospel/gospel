(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Types

type tvar = Ident.t

type tsymbol = { ts_id : Ident.t; ts_ty : Types.ty }
(** Typed variables *)

(** Typed variables *)
let mk_ts ts_id ts_ty = { ts_id; ts_ty }

type pat = { pat_desc : pat_desc; pat_loc : Location.t }

and pat_desc =
  | Pwild
  | Pid of tsymbol
  | Ptuple of pat list
  | Pcast of pat * Types.ty

(** Typed terms *)
type term_node =
  | Ttrue
  | Tfalse
  | TTrue
  | TFalse
  | Tvar of Id_uast.qualid
  | Tlet of pat * term * term
  | Tconst of Parse_uast.constant
  | Tapply of term * term
  | Ttyapply of Id_uast.qualid * Id_uast.pty list
  | Tquant of Parse_uast.quant * tsymbol list * term
  | Tif of term * term * term
  | Ttuple of term list
  | Tlambda of pat list * term * Id_uast.pty option
  | Trecord of (Id_uast.qualid * term) list
  | Tfield of term * Id_uast.qualid
  | Tattr of string * term
  | Tcast of term * Id_uast.pty
  | Tscope of Id_uast.qualid * term
  | Told of term

and term = { t_node : term_node; t_ty : ty; t_loc : Location.t }

let mk_term t_node t_ty t_loc = { t_node; t_ty; t_loc }

(* Typed Signatures *)

type axiom = {
  ax_name : Ident.t;  (** Name *)
  ax_term : term;  (** Definition *)
  ax_tvars : tvar list;  (** Type variables *)
  ax_loc : Location.t;  (** Location *)
  ax_text : string;
      (** String containing the original specificaion as written by the user *)
}

type fun_spec = {
  fun_req : term list;  (** Preconditions *)
  fun_ens : term list;  (** Postconditions *)
  fun_variant : term list;  (** Variant *)
  fun_text : string;
      (** String containing the original specificaion as written by the user *)
  fun_loc : Location.t;  (** Specification location *)
}

type function_ = {
  fun_name : Ident.t;  (** Function symbol *)
  fun_rec : bool;  (** Recursive *)
  fun_params : tsymbol list;  (** Arguments *)
  fun_tvars : tvar list;  (** *)
  fun_ret : ty;
  fun_def : term option;  (** Definition *)
  fun_spec : fun_spec;  (** Specification *)
  fun_loc : Location.t;  (** Location *)
}

type type_spec = {
  ty_mutable : bool;
  ty_invariant : (Id_uast.id * term list) option;
  ty_model : Id_uast.model;
  ty_text : string;
  ty_loc : Location.t;
}

let mk_type_spec ty_mutable ty_invariant ty_model ty_text ty_loc =
  { ty_mutable; ty_invariant; ty_model; ty_text; ty_loc }

let empty_tspec = mk_type_spec false None No_model "" Location.none

type s_type_declaration = {
  tname : Id_uast.id;
  tparams : tvar list;
  tkind : Id_uast.type_kind;
  tmanifest : Id_uast.pty option;
  tattributes : Ppxlib.attributes;
  (* ... [@@id1] [@@id2] *)
  tspec : type_spec;
  (* specification *)
  tloc : Location.t;
}

let mk_tdecl tname tparams tkind tmanifest tattributes tspec tloc =
  { tname; tparams; tkind; tmanifest; tattributes; tspec; tloc }

type xpost_spec = {
  sp_exn : Id_uast.qualid;
  sp_xargs : Id_uast.sp_var list;
  sp_xrets : Id_uast.sp_var list;
  sp_xtops : Id_uast.ocaml_sp_var list;
  sp_xpost : term list;
  sp_xloc : Location.t;
}

type val_spec = {
  sp_args : Id_uast.sp_var list;
  sp_rets : Id_uast.sp_var list;
  sp_tops : Id_uast.ocaml_sp_var list;
  sp_pre : term list;
  sp_checks : term list;
  sp_post : term list;
  sp_xspec : xpost_spec list;
  sp_diverge : bool;
  sp_pure : bool;
  sp_text : string;
  sp_loc : Location.t;
}

type s_val_description = {
  vname : Ident.t;
  vtype : ty;
  vtvars : tvar list;
  (* OCaml type of the value *)
  vattributes : Ppxlib.attributes;
  (* ... [@@id1] [@@id2] *)
  vspec : val_spec option;
  (* specification *)
  vloc : Location.t;
}

type s_module_type_desc = Mod_signature of s_signature

and s_module_declaration = {
  mdname : Ident.t;
  mdtype : s_module_type;
  mdattributes : Ppxlib.attributes;
  (* ... [@@id1] [@@id2] *)
  mdloc : Location.t;
}

and s_module_type = {
  mdesc : s_module_type_desc;
  mloc : Location.t;
  mattributes : Ppxlib.attributes; (* ... [@id1] [@id2] *)
}

and s_signature_item_desc =
  | Sig_value of s_val_description
  | Sig_function of function_
  | Sig_axiom of axiom
  | Sig_module of s_module_declaration
  | Sig_ghost_type of s_type_declaration list
  | Sig_type of s_type_declaration list
  | Sig_ghost_open of Id_uast.qualid
  | Sig_exception of Id_uast.exception_decl
  | Sig_attribute of Ppxlib.attribute

and s_signature_item = { sdesc : s_signature_item_desc; sloc : Location.t }
and s_signature = s_signature_item list

(* Helper functions *)

let mk_fun_spec fun_req fun_ens fun_variant fun_text fun_loc =
  { fun_req; fun_ens; fun_variant; fun_text; fun_loc }

let mk_function f fun_params fun_def fun_ret fun_spec =
  {
    fun_name = f.Id_uast.fun_name;
    fun_rec = f.fun_rec;
    fun_params;
    fun_tvars = [];
    fun_def;
    fun_ret;
    fun_spec;
    fun_loc = f.fun_loc;
  }

let fun_to_arrow args ret =
  List.fold_right (fun arg ret -> Types.ty_arrow arg.ts_ty ret) args ret

let mk_axiom ax_name ax_term ax_loc ax_text =
  { ax_name; ax_term; ax_tvars = []; ax_loc; ax_text }

let mk_xpost sp_exn sp_xargs sp_xrets sp_xtops sp_xpost sp_xloc =
  { sp_exn; sp_xargs; sp_xrets; sp_xtops; sp_xpost; sp_xloc }

let mk_vspec sp_args sp_rets sp_tops sp_pre sp_checks sp_post sp_xspec
    sp_diverge sp_pure sp_text sp_loc =
  {
    sp_args;
    sp_rets;
    sp_tops;
    sp_pre;
    sp_checks;
    sp_post;
    sp_xspec;
    sp_diverge;
    sp_pure;
    sp_text;
    sp_loc;
  }
