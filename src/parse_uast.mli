(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

type id = Preid.t

(* Types *)
open Ppxlib

type qualid = Qid of id | Qdot of qualid * id

type pty =
  | PTtyvar of id
  | PTtyapp of qualid * pty list
  | PTtuple of pty list
  | PTarrow of pty * pty

type labelled_arg =
  | Lwild
  | Lunit of Location.t
  | Lvar of id
  | Lghost of id * pty

(* Logical terms and formulas *)

type binder = id * pty option

type pat = { pat_desc : pat_desc; pat_loc : Location.t }
and pat_desc = Pwild | Pid of id | Ptuple of pat list | Pcast of pat * pty

type param = id * pty
type quant = Tforall | Texists

type constant =
  | Pconst_integer of string * char option
  | Pconst_char of char
  | Pconst_string of string * Location.t
  | Pconst_float of string

type term = { term_desc : term_desc; term_loc : Location.t }

and term_desc =
  | Ttrue
  | Tfalse
  | TTrue
  | TFalse
  | Tconst of constant
  | Tvar of qualid
  | Tfield of term * qualid
  | Tapply of term * term
  | Tinfix of term * id * term
  (* [Tinfix] represents a chain of infix operations such as [e1 <
       e2 < ...]. This node is necessary during parsing so that there
       exists a distinction between chains of infix operators such
       as (x < y < z) and (x < (y < z)). During typing, this node is
       desugared into a normal function application and handled the
       same way as [Tapply] *)
  | Tif of term * term * term
  | Tquant of quant * binder list * term
  | Tlambda of pat list * term * pty option
  | Tattr of string * term
  | Tlet of pat * term * term
  | Tcast of term * pty
  | Ttuple of term list
  | Trecord of (qualid * term) list
  | Tscope of qualid * term
  | Told of term

(* Specification *)

type spec_header = {
  sp_hd_nm : id;
  (* header name *)
  sp_hd_ret : labelled_arg list;
  (* Can only be LNone or LGhost *)
  sp_hd_args : labelled_arg list; (* header arguments' names *)
}

type pre_spec = {
  sp_pre : term list;
  sp_checks : term list;
  sp_consumes : qualid list;
  sp_modifies : qualid list;
  sp_preserves : qualid list;
  sp_diverge : bool;
  sp_pure : bool;
}

type post_spec = { sp_post : term list; sp_produces : qualid list }

type xpost_spec = {
  sp_exn : qualid;
  sp_xrets : labelled_arg list;
  sp_xpost : term list;
  sp_xproduces : qualid list;
  sp_xloc : Location.t;
}

type val_spec = {
  sp_header : spec_header option;
  sp_pre_spec : pre_spec;
  sp_post_spec : post_spec;
  sp_xpost_spec : xpost_spec list;
  sp_text : string;
  sp_loc : Location.t;
}

type model = No_model | Implicit of pty | Fields of (id * pty) list

type type_spec = {
  ty_mutable : bool;
  ty_invariant : (id * term list) option;
  ty_model : model;
  ty_text : string;
  ty_loc : Location.t;
}

type fun_spec = {
  fun_req : term list;
  fun_ens : term list;
  fun_variant : term list;
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
  fun_spec : fun_spec;
  fun_loc : Location.t;
}

type axiom = {
  ax_name : id;
  ax_term : term;
  ax_loc : Location.t;
  ax_text : string;
}

(* Modified OCaml constructs with specification attached *)

type s_val_description = {
  vname : id;
  vtype : pty;
  vattributes : attributes;
  (* ... [@@id1] [@@id2] *)
  vspec : val_spec option;
  (* specification *)
  vloc : Location.t;
}

type mutable_flag = Mutable | Immutable

type label_declaration = {
  pld_name : id;
  pld_mutable : mutable_flag;
  pld_type : pty;
  pld_loc : Location.t;
}

type type_kind = PTtype_abstract | PTtype_record of label_declaration list
type private_flag = Private | Public

type s_type_declaration = {
  tname : id;
  tparams : id list;
  tkind : type_kind;
  tmanifest : pty option;
  tattributes : attributes;
  (* ... [@@id1] [@@id2] *)
  tspec : type_spec option;
  (* specification *)
  tloc : Location.t;
}

type gospel_signature =
  | Sig_function of function_
  | Sig_axiom of axiom
  | Sig_ghost_type of s_type_declaration list
  | Sig_ghost_open of qualid

type exception_decl = {
  exn_id : id;
  exn_args : pty list;
  exn_loc : Location.t;
  exn_attributes : attributes;
}

type s_signature_item_desc =
  | Sig_val of s_val_description
  (*
          val x: T
          external x: T = "s1" ... "sn"
         *)
  | Sig_type of s_type_declaration list
  (* type t1 = ... and ... and tn = ... *)
  | Sig_module of s_module_declaration
  (* module X : MT *)
  | Sig_exception of exception_decl
  (* exception C of T *)
  | Sig_open of qualid
  (* open X *)
  | Sig_attribute of attribute
  (* [@@@id] *)
  | Sig_gospel of gospel_signature * string

and s_signature_item = { sdesc : s_signature_item_desc; sloc : Location.t }
and s_signature = s_signature_item list
and s_module_type_desc = Mod_signature of s_signature

and s_module_type = {
  mdesc : s_module_type_desc;
  mloc : Location.t;
  mattributes : attributes; (* ... [@id1] [@id2] *)
}

and s_module_declaration = {
  mdname : id;
  mdtype : s_module_type;
  mdattributes : attributes;
  (* ... [@@id1] [@@id2] *)
  mdloc : Location.t;
}
