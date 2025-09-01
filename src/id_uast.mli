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
  | PTtyapp of app_info * pty list
  | PTtuple of pty list
  | PTarrow of pty * pty

and app_info = {
  app_qid : qualid;
  app_alias : pty option;
  app_model : pty option;
  app_mut : bool;
}
(** For every type application the user writes, we also keep track of the alias
    for that type. We need the type alias for the type checking phase and we
    need the original annotation for the so that we preserve what the user wrote
    across in the typed AST. If [app_alias] in [None], this type application
    does not have a type alias.

    Invariant : If a type application has the type alias [t], all type
    applications used within [t] will not have any aliases.

    Invariant: If a type application has the model [t], all type application
    used within [t] will not have any models. *)

(* Logical terms and formulas *)

type binder = id * pty option
type param = id * pty

type pat = { pat_desc : pat_desc; pat_loc : Location.t }
and pat_desc = Pwild | Pid of id | Ptuple of pat list | Pcast of pat * pty

type ty_app = { params : id list; name : id }
(** Name of a type coupled with its type variables. *)

type term = { term_desc : term_desc; term_loc : Location.t }

and term_desc =
  | Ttrue
  | Tfalse
  | TTrue
  | TFalse
  | Tconst of Parse_uast.constant
  | Tvar of qualid * id list * pty
    (* We have two nodes for term variables, one for local variables and another
       for top level variables. This is necessary because the Inferno solver can
       only keep track of types for variables that are local to the given term,
       meaning we must supply the type of all top level variables that the term
       uses. *)
  | Tlocal of id
  | Tfield of term * ty_app * qualid * pty
    (* For field applications, we store the type of the record it belongs to as
       well as the type of the record field. *)
  | Tapply of term * term
  | Tif of term * term * term
  | Tset of id * term
  | Tquant of Parse_uast.quant * binder list * term
  | Tlambda of pat list * term * pty option
  | Tattr of string * term
  | Tlet of pat * term * term
  | Tcast of term * pty
  | Ttuple of term list
  | Trecord of (qualid * term * pty) list * ty_app
  | Tscope of qualid * term
  | Told of term

(* Specification *)

type ocaml_sp_var = {
  var_name : qualid; (* Variable name *)
  ty_ocaml : pty; (* OCaml type of the variable. *)
  ty_gospel : pty option; (* Gospel type of the variable. *)
  prod : bool;
  (* Flag indicating if the function receives ownership of the value. *)
  cons : bool;
  (* Flag indicating if the function returns ownership of the value. *)
  ro : bool;
      (* Read only flag. If [false], the variable is modified
           by the function.

           Remark: If this [sp_var] value refers to a return
           value, [ro] is [true]. *)
}

type sp_var =
  | Wildcard
  | Unit
  | Ghost of id * pty (* Ghost variable *)
  | OCaml of ocaml_sp_var

type xpost_spec = {
  sp_exn : qualid;
  sp_xargs : sp_var list;
  sp_xrets : sp_var list;
  sp_xtops : ocaml_sp_var list;
  sp_xpost : term list;
  sp_xloc : Location.t;
}

type val_spec = {
  sp_args : sp_var list;
  sp_rets : sp_var list;
  sp_tops : ocaml_sp_var list;
  sp_pre : term list;
  sp_checks : term list;
  sp_post : term list;
  sp_xpost : xpost_spec list;
  sp_diverge : bool;
  sp_pure : bool;
  sp_text : string;
  sp_loc : Location.t;
}

type model = No_model | Implicit of pty | Fields of (id * pty) list

type type_spec = {
  ty_ephemeral : bool;
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

type label_declaration = {
  pld_name : id;
  pld_mutable : Parse_uast.mutable_flag;
  pld_type : pty;
  pld_loc : Location.t;
}

type type_kind = PTtype_abstract | PTtype_record of label_declaration list

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
  | Sig_ghost_type of s_type_declaration
  | Sig_ghost_open of qualid

type exception_decl = {
  exn_id : id;
  exn_args : pty list;
  exn_loc : Location.t;
  exn_attributes : attributes;
}

type s_signature_item_desc =
  | Sig_type of rec_flag * s_type_declaration list
  (* type t1 = ... and ... and tn = ... *)
  | Sig_module of s_module_declaration
  (* module X : MT *)
  | Sig_exception of exception_decl
  (* exception C of T *)
  | Sig_attribute of attribute
  (* [@@@id] *)
  (* Specific to specification *)
  | Sig_gospel of gospel_signature * string

and s_signature_item = { sdesc : s_signature_item_desc; sloc : Location.t }
and s_signature = s_signature_item list
and s_module_type_desc = Mod_ident of Longident.t loc

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
