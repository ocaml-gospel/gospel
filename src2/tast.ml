open Ppxlib
open Common

let (_ : Uast.ghost) = true

type val_spec = {
  (* arguments are symbols (with type and ghost status) and their label *)
  sp_args : Symbols.symbol labelled list;
  (* the returned value is coded with a pattern, not a list of labelled symbols as in V1
     we have pattern for tuples, so it's good *)
  sp_ret : Tterm.pattern;
  (* the following is the same as in V1 *)
  sp_pre : Tterm.terms;
  sp_checks : Tterm.terms;
  sp_post : Tterm.terms;
  sp_xpost : (Symbols.xsymbol * cases) list;
  (* except sp_wr -> sp_mod because it is a `modifies' clause *)
  sp_mod : Tterm.terms;
  sp_cs : Tterm.terms;
  sp_diverge : bool;
  sp_pure : bool;
  sp_equiv : string list;
  sp_text : string;
  loc : Location.t;
}

type val_desc = {
  vd_name : Identifier.Ident.t;
  (* a Gospel type here rather than a Parsetree.core_type, see if it is a good idea *)
  vd_type : Ttypes.ty;
  vd_prim : string list;
  vd_attrs : Parsetree.attributes;
  (* V1 store here args and ret, but they need to be build if there is no spec *)
  vd_spec : val_spec option;
  vd_loc : Location.t;
}

type mutable_flag = Immutable | Mutable

type type_spec = {
  ty_mutable : mutable_flag;
  ty_fields : (Symbols.symbol * mutable_flag) list;
  ty_invariants : Tterm.terms;
  ty_text : string;
  ty_loc : Location.t;
}

type label_declaration = {
  ld_name : Symbols.symbol;
  ld_mutable : mutable_flag;
  ld_type : Ttypes.ty;
  ld_loc : Location.t;
  ld_attributes : Parsetree.attributes;
}

type constructor_argument =
  | Cstr_tuple of Ttypes.ty list
  | Cstr_record of label_declaration list

type constructor_arguments = constructor_argument list

type constructor_declaration = {
  cd_name : Symbols.symbol;
  cd_args : constructor_arguments;
  (* what is that ? *)
  cd_res : Ttypes.ty option;
  cd_loc : Location.t;
  cd_attributes : Parsetree.attributes;
}

type type_kind =
  | Abstract
  | Variant of constructor_declaration list
  | Record of label_declaration list
(* OCaml has a Ptype_open but it seems we don't need it *)

type type_decl = {
  (* Ocaml use a string (with location) here *)
  td_ts : Ttypes.tsymbol;
  (* Why3 uses params  *)
  td_params : (Ttypes.tsymbol * (variance * injectivity)) list;
  (* do we need the informations about the constraints ? *)
  td_cstrs : (Ttypes.ty * Ttypes.ty * Location.t) list;
  td_kind : type_kind;
  (* and about private status ? *)
  td_private : private_flag;
  td_manifest : Ttypes.ty option;
  td_attrs : Parsetree.attributes;
  td_spec : type_spec option;
  td_loc : Location.t;
}

type axiom = {
  ax_name : Identifier.Ident.t;
  ax_def : Tterm.term;
  ax_loc : Location.t;
  ax_text : string;
}

type fun_spec = {
  fun_pre : Tterm.terms;
  fun_post : Tterm.terms;
  fun_variant : Tterm.terms;
  fun_coerc : bool;
  fun_test : string;
  fun_loc : Location.t;
}

type function_ = {
  fun_name : Symbols.symbol; (* must be a Logical *)
  fun_rec : bool;
  fun_params : Symbols.symbol list;
  fun_def : Tterm.term option;
  fun_spec : fun_spec option;
  fun_text : string;
  fun_loc : Location.t;
}

(*----------------------*)
(* same as V1 from here *)
(* modulo some names    *)
(*----------------------*)

type extension_constructor = {
  ext_ident : Identifier.Ident.t;
  ext_xs : Symbols.xsymbol;
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
  | Wty of Identifier.Ident.t * type_decl
  (* with type X.t = ...
     Note: the last component of the longIdentifier.t must match
     the name of the type_declaration. *)
  | Wmod of Identifier.Ident.t * Identifier.Ident.t
  (* with module X.Y = Z *)
  | Wtysubs of Identifier.Ident.t * type_decl
  (* with type X.t := ..., same format as [Pwith_type] *)
  | Wmodsubs of Identifier.Ident.t * Identifier.Ident.t
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
  | Sig_val of val_desc * ghost
  | Sig_type of rec_flag * type_decl list * ghost
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
  (* type t1 += ... *)
  (* include MT *)
  | Sig_class of Parsetree.class_description list
  (* type t1 += ... *)
  (* class c1 : ... and ... and cn : ... *)
  | Sig_class_type of Parsetree.class_type_declaration list
  (* type t1 += ... *)
  (* class type ct1 = ... and ... and ctn = ... *)
  | Sig_attribute of Parsetree.attribute
  (* type t1 += ... *)
  (* [@@@id] *)
  | Sig_extension of Parsetree.extension * Parsetree.attributes
  (* type t1 += ... *)
  (* [%%id] *)
  (* Specific to specification *)
  | Sig_use of string
  | Sig_function of function_
  | Sig_axiom of axiom

and module_declaration = {
  md_name : Identifier.Ident.t;
  md_type : module_type;
  md_attrs : Parsetree.attributes;
  (* ... [@@id1] [@@id2] *)
  md_loc : Location.t;
}

and module_type_declaration = {
  mtd_name : Identifier.Ident.t;
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
  | Mod_functor of Identifier.Ident.t * module_type option * module_type
  (* functor(X : MT1) -> MT2 *)
  | Mod_with of module_type * with_constraint list
  (* MT with ... *)
  | Mod_typeof of Parsetree.module_expr
  (* module type of ME *)
  | Mod_extension of Parsetree.extension
  (* [%id] *)
  | Mod_alias of string list
(* (module M) *)
