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
open Asttypes
open Parsetree

module Preid = Identifier.Preid

(* Types *)

type qualid =
  | Qpreid of Preid.t
  | Qdot   of qualid * Preid.t

type pty =
  | PTtyvar  of Preid.t
  | PTtyapp  of qualid * pty list
  | PTtuple  of pty list
  | PTarrow  of labelled_arg * pty * pty

and labelled_arg =
  | Lunit
  | Lnone     of Preid.t
  | Loptional of Preid.t
  | Lnamed    of Preid.t
  | Lghost    of Preid.t * pty

(* Patterns *)

type ghost = bool

type pattern = {
  pat_desc : pat_desc;
  pat_loc  : Location.t;
}

and pat_desc =
  | Pwild
  | Pvar   of Preid.t
  | Papp   of qualid * pattern list
  | Prec   of (qualid * pattern) list
  | Ptuple of pattern list
  | Pas    of pattern * Preid.t
  | Por    of pattern * pattern
  | Pcast  of pattern * pty

(* Logical terms and formulas *)

type binder = Preid.t * pty option
type param  = Location.t * Preid.t * pty

type binop = Tand | Tand_asym | Tor | Tor_asym | Timplies | Tiff
type quant = Tforall | Texists | Tlambda

type term = {
  term_desc : term_desc;
  term_loc  : Location.t;
}

and term_desc =
  | Ttrue
  | Tfalse
  | Tconst  of constant
  | Tpreid  of qualid
  | Tidapp  of qualid * term list
  | Tapply  of term * term
  | Tinfix  of term * Preid.t * term
  | Tbinop  of term * binop * term
  | Tnot    of term
  | Tif     of term  * term * term
  | Tquant  of quant * binder list * term list list * term
  | Tattr   of string * term
  | Tlet    of Preid.t * term * term
  | Tcase   of term  * (pattern * term) list
  | Tcast   of term  * pty
  | Ttuple  of term list
  | Trecord of (qualid * term) list
  | Tupdate of term * (qualid * term) list
  | Tscope  of qualid * term
  | Told    of term

(* Specification *)

type xpost = Location.t * (qualid * (pattern * term) option) list

type val_spec = {
  sp_hd_nm   : Preid.t;           (* header name *)
  sp_hd_ret  : labelled_arg list; (* Can only be LNone or LGhost *)
  sp_hd_args : labelled_arg list; (* header arguments' names *)
  sp_pre     : term list;
  sp_checks  : term list;
  sp_post    : term list;
  sp_xpost   : xpost list;
  sp_writes  : term list;
  sp_consumes: term list;
  sp_diverge : bool;
  sp_pure    : bool;
  sp_equiv   : string list;
}

type field = {
  f_loc     : Location.t;
  f_preid   : Preid.t;
  f_pty     : pty;
  f_mutable : bool;
}

type type_spec = {
  ty_ephemeral : bool;
  ty_field     : field list;
  ty_invariant : term list;
}

type fun_spec = {
  fun_req     : term list;
  fun_ens     : term list;
  fun_variant : term list;
  fun_coer    : bool;
}

type loop_spec = {
  loop_invariant : term list;
  loop_variant   : term list;
}

(* type param  = Location.t * Preid.t * pty *)
type function_ = {
  fun_name    : Preid.t;
  fun_rec     : bool;
  fun_type    : pty option;
  fun_params  : param list;
  fun_def     : term option;
  fun_spec    : fun_spec option;
  fun_loc     : Location.t;
}

type axiom = {
  ax_name : Preid.t;
  ax_term : term;
  ax_loc  : Location.t
}

(* TODO: have only one type for axioms *)
type prop_kind = Plemma | Paxiom

type prop = {
  prop_name : Preid.t;
  prop_term : term;
  prop_loc  : Location.t;
  prop_kind : prop_kind;
}

(* Modified OCaml constructs with specification attached *)

type s_val_description =
  {
    vname       : string loc;
    vtype       : core_type;
    vprim       : string list;
    vattributes : attributes;  (* ... [@@id1] [@@id2] *)
    vspec       : val_spec option; (* specification *)
    vloc        : Location.t;
  }

type s_type_declaration =
  {
    tname       : string loc;
    tparams     : (core_type * (variance * injectivity)) list;
    (* ('a1,...'an) t; None represents  _*)
    tcstrs      : (core_type * core_type * Location.t) list;
    (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
    tkind       : type_kind;
    tprivate    : private_flag;   (* = private ... *)
    tmanifest   : core_type option;  (* = T *)
    tattributes : attributes;   (* ... [@@id1] [@@id2] *)
    tspec       : type_spec option; (* specification *)
    tloc        : Location.t;
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
  | Sig_typext of type_extension
        (* type t1 += ... *)
  | Sig_module of s_module_declaration
        (* module X : MT *)
  | Sig_recmodule of s_module_declaration list
        (* module rec X1 : MT1 and ... and Xn : MTn *)
  | Sig_modtype of s_module_type_declaration
        (* module type S = MT
           module type S *)
  (* these were not modified *)
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
  | Sig_ghost_type  of rec_flag * s_type_declaration list
  | Sig_ghost_val   of s_val_description
  | Sig_ghost_open  of open_description

and s_signature_item =
  {
    sdesc : s_signature_item_desc;
    sloc  : Location.t;
  }

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

and s_module_expr = {
  spmod_desc: s_module_expr_desc;
  spmod_loc: Location.t;
  spmod_attributes: attributes; (* ... [@id1] [@id2] *)
}

and s_module_type =
  {
    mdesc       : s_module_type_desc;
    mloc        : Location.t;
    mattributes : attributes; (* ... [@id1] [@id2] *)
  }

and s_module_declaration =
  {
    mdname       : string option loc;
    mdtype       : s_module_type;
    mdattributes : attributes; (* ... [@@id1] [@@id2] *)
    mdloc        : Location.t;
  }

and s_module_type_declaration =
    {
     mtdname       : string loc;
     mtdtype       : s_module_type option;
     mtdattributes : attributes; (* ... [@@id1] [@@id2] *)
     mtdloc        : Location.t;
    }

and s_expression = {
  spexp_desc: s_expression_desc;
  spexp_loc: Location.t;
  spexp_loc_stack: Location.t list;
  spexp_attributes: attributes; (* ... [@id1] [@id2] *)
}

and s_expression_desc =
  | Sexp_ident of Longident.t loc
        (* x
           M.x
         *)
  | Sexp_constant of constant
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Sexp_let of rec_flag * s_value_binding list * s_expression
        (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Sexp_function of s_case list
        (* function P1 -> E1 | ... | Pn -> En *)
  | Sexp_fun of
      arg_label * s_expression option * Parsetree.pattern * s_expression *
      fun_spec option
        (* fun P -> E1                          (Simple, None)
           fun ~l:P -> E1                       (Labelled l, None)
           fun ?l:P -> E1                       (Optional l, None)
           fun ?l:(P = E0) -> E1                (Optional l, Some E0)

           Notes:
           - If E0 is provided, only Optional is allowed.
           - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
           - "let f P = E" is represented using Pexp_fun.
         *)
  | Sexp_apply of s_expression * (arg_label * s_expression) list
        (* E0 ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
         *)
  | Sexp_match of s_expression * s_case list
        (* match E0 with P1 -> E1 | ... | Pn -> En *)
  | Sexp_try of s_expression * s_case list
        (* try E0 with P1 -> E1 | ... | Pn -> En *)
  | Sexp_tuple of s_expression list
        (* (E1, ..., En)

           Invariant: n >= 2
        *)
  | Sexp_construct of Longident.t loc * s_expression option
        (* C                None
           C E              Some E
           C (E1, ..., En)  Some (Sexp_tuple[E1;...;En])
        *)
  | Sexp_variant of label * s_expression option
        (* `A             (None)
           `A E           (Some E)
         *)
  | Sexp_record of (Longident.t loc * s_expression) list * s_expression option
        (* { l1=P1; ...; ln=Pn }     (None)
           { E0 with l1=P1; ...; ln=Pn }   (Some E0)

           Invariant: n > 0
         *)
  | Sexp_field of s_expression * Longident.t loc
        (* E.l *)
  | Sexp_setfield of s_expression * Longident.t loc * s_expression
        (* E1.l <- E2 *)
  | Sexp_array of s_expression list
        (* [| E1; ...; En |] *)
  | Sexp_ifthenelse of s_expression * s_expression * s_expression option
        (* if E1 then E2 else E3 *)
  | Sexp_sequence of s_expression * s_expression
        (* E1; E2 *)
  | Sexp_while of s_expression * s_expression * loop_spec option
        (* while E1 do E2 done *)
  | Sexp_for of
      Parsetree.pattern * s_expression * s_expression * direction_flag *
      s_expression * loop_spec option (* TODO: we do not need the variant *)
        (* for i = E1 to E2 do E3 done      (flag = Upto)
           for i = E1 downto E2 do E3 done  (flag = Downto)
         *)
  | Sexp_constraint of s_expression * core_type
        (* (E : T) *)
  | Sexp_coerce of s_expression * core_type option * core_type
        (* (E :> T)        (None, T)
           (E : T0 :> T)   (Some T0, T)
         *)
  | Sexp_send of s_expression * label loc
        (*  E # m *)
  | Sexp_new of Longident.t loc
        (* new M.c *)
  | Sexp_setinstvar of label loc * s_expression
        (* x <- 2 *)
  | Sexp_override of (label loc * s_expression) list
        (* {< x1 = E1; ...; Xn = En >} *)
  | Sexp_letmodule of string option loc * module_expr * s_expression
        (* let module M = ME in E *)
  | Sexp_letexception of extension_constructor * s_expression
        (* let exception C in E *)
  | Sexp_assert of s_expression
        (* assert E
           Note: "assert false" is treated in a special way by the
           type-checker. *)
  | Sexp_lazy of s_expression
        (* lazy E *)
  | Sexp_poly of s_expression * core_type option
        (* Used for method bodies.

           Can only be used as the s_expression under Cfk_concrete
           for methods (not values). *)
  | Sexp_object of class_structure
        (* object ... end *)
  | Sexp_newtype of string loc * s_expression
        (* fun (type t) -> E *)
  | Sexp_pack of s_module_expr
        (* (module ME)

           (module ME : S) is represented as
           Sexp_constraint(Sexp_pack, Ptyp_package S) *)
  | Sexp_open of open_declaration * s_expression
        (* M.(E)
           let open M in E
           let! open M in E *)
  | Sexp_letop of letop
  | Sexp_extension of extension
        (* [%id] *)
  | Sexp_unreachable
        (* . *)

and s_case = {   (* (P -> E) or (P when E0 -> E) *)
  spc_lhs: Parsetree.pattern;
  spc_guard: s_expression option;
  spc_rhs: s_expression;
}

and s_module_expr_desc =
  | Smod_ident of Longident.t loc
        (* X *)
  | Smod_structure of s_structure
        (* struct ... end *)
  | Smod_functor of string option loc * s_module_type option * s_module_expr
        (* functor(X : MT1) -> ME *)
  | Smod_apply of s_module_expr * s_module_expr
        (* ME1(ME2) *)
  | Smod_constraint of s_module_expr * s_module_type
        (* (ME : MT) *)
  | Smod_unpack of s_expression
        (* (val E) *)
  | Smod_extension of extension
        (* [%id] *)

and s_structure = s_structure_item list

and s_structure_item = {
  sstr_desc: s_structure_item_desc;
  sstr_loc: Location.t;
}

and s_structure_item_desc =
  | Str_eval of s_expression * attributes
        (* E *)
  | Str_value of rec_flag * s_value_binding list
        (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
         *)
  | Str_primitive of s_val_description
        (*  val x: T
            external x: T = "s1" ... "sn" *)
  | Str_type of rec_flag * s_type_declaration list
        (* type t1 = ... and ... and tn = ... *)
  | Str_typext of type_extension
        (* type t1 += ... *)
  | Str_exception of type_exception
        (* exception C of T
           exception C = M.X *)
  | Str_module of s_module_binding
        (* module X = ME *)
  | Str_recmodule of s_module_binding list
        (* module rec X1 = ME1 and ... and Xn = MEn *)
  | Str_modtype of s_module_type_declaration
        (* module type S = MT *)
  | Str_open of open_description
        (* open X *)
  | Str_class of class_declaration list
        (* class c1 = ... and ... and cn = ... *)
  | Str_class_type of class_type_declaration list
        (* class type ct1 = ... and ... and ctn = ... *)
  | Str_include of include_declaration
        (* include ME *)
  | Str_attribute of attribute
        (* [@@@id] *)
  | Str_extension of extension * attributes
        (* [%%id] *)
  (* Specific to specification *)
  | Str_function of function_
  | Str_prop of prop
  | Str_ghost_type of rec_flag * s_type_declaration list
  | Str_ghost_val  of s_val_description
  | Str_ghost_open of open_declaration

and s_value_binding = {
  spvb_pat: Parsetree.pattern; (* FIXME: change this pattern type *)
  spvb_expr: s_expression;
  spvb_attributes: attributes;
  spvb_vspec: val_spec option;
  spvb_loc: Location.t;
}

and s_module_binding = {
  spmb_name: string loc;
  spmb_expr: s_module_expr;
  spmb_attributes: attributes;
  spmb_loc: Location.t;
}
