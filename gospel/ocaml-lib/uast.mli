open Identifier
open Oasttypes
open Oparsetree

(* Types *)

type qualid =
  | Qpreid of preid
  | Qdot   of qualid * preid

(* SUGGESTION maybe the core can be used instead *)
type pty =
  | PTtyvar  of preid
  | PTtyapp  of qualid * pty list
  | PTtuple  of pty list
  | PTarrow  of labelled_arg * pty * pty

and labelled_arg =
  | Lnone     of preid
  | Lquestion of preid
  | Lnamed    of preid
  | Lghost    of preid * pty

(* Patterns *)

type ghost = bool

type pattern = {
  pat_desc : pat_desc;
  pat_loc  : Location.t;
}

and pat_desc =
  | Pwild
  | Pvar   of preid
  | Papp   of qualid * pattern list
  | Prec   of (qualid * pattern) list
  | Ptuple of pattern list
  | Pas    of pattern * preid
  | Por    of pattern * pattern
  | Pcast  of pattern * pty
  (* | Pscope of qualid * pattern TODO: think about *)

(* Logical terms and formulas *)

type binder = preid * pty option
type param  = Location.t * preid * pty

type binop = Tand | Tand_asym | Tor | Tor_asym | Timplies | Tiff
  (* TODO: think about 'by' and 'so' *)
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
  | Tinfix  of term * preid * term
  | Tbinop  of term * binop * term
  | Tnot    of term
  | Tif     of term  * term * term
  | Tquant  of quant * binder list * term list list * term
  | Tattr   of attr  * term
  | Tlet    of preid * term * term
  | Tcase   of term  * (pattern * term) list
  | Tcast   of term  * pty
  | Ttuple  of term list
  | Trecord of (qualid * term) list
  | Tupdate of term * (qualid * term) list
  | Tscope  of qualid * term
  | Told    of term

(* Specification *)

type invariant = term list

type pre = term * bool (* wether it is a checks *)
type post = term
type xpost = Location.t * (qualid * (pattern * term) option) list

type val_spec = {
    sp_hd_nm   : preid;             (* header name *)
    sp_hd_ret  : labelled_arg list; (* Can only be LNone or LGhost *)
    sp_hd_args : labelled_arg list; (* header arguments' names *)
    sp_pre     : pre list;
    sp_post    : post list;
    sp_xpost   : xpost list;
    sp_reads   : qualid list;        (* TODO *)
    sp_writes  : term list;
    sp_alias   : (term * term) list; (* TODO *)
    sp_diverge : bool;
    sp_equiv   : string list;
}

type field = {
  f_loc     : Location.t;
  f_preid   : preid;
  f_pty     : pty;
  f_mutable : bool;
}

type type_spec = {
  ty_ephemeral : bool;
  ty_field     : field list;
  ty_invariant : invariant;
}

type fun_spec = {
  fun_req     : term list;
  fun_ens     : term list;
  fun_variant : term list;
  fun_coer    : bool;
}

(* type param  = Location.t * preid * pty *)
type function_ = {
  fun_name    : preid;
  fun_rec     : bool;
  fun_type    : pty option;
  fun_params  : param list;
  fun_def     : term option;
  fun_spec    : fun_spec;
  fun_loc     : Location.t;
}

type axiom = {
  ax_name : preid;
  ax_term : term;
  ax_loc  : Location.t
}

type spec =
  | Stype       of type_spec * Location.t
  | Sval        of val_spec * Location.t
  | Suse        of qualid * Location.t
  | Sfunction   of function_ * Location.t
  | Sfunc_spec  of fun_spec * Location.t
  | Saxiom      of axiom * Location.t
  | Stype_ghost of rec_flag * type_declaration list * Location.t
  | Sval_ghost  of value_description  * Location.t

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
    tparams     : (core_type * variance) list;
    (* ('a1,...'an) t; None represents  _*)
    tcstrs      : (core_type * core_type * Location.t) list;
    (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
    tkind       : type_kind;
    tprivate    : private_flag;   (* = private ... *)
    tmanifest   : core_type option;  (* = T *)
    tattributes : attributes;   (* ... [@@id1] [@@id2] *)
    tspec       : type_spec; (* specification *)
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
  | Sig_use of qualid
  | Sig_function of function_
  | Sig_axiom of axiom
  | Sig_ghost_type  of rec_flag * s_type_declaration list
  | Sig_ghost_val of s_val_description

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
  | Mod_functor of string loc * s_module_type option * s_module_type
        (* functor(X : MT1) -> MT2 *)
  | Mod_with of s_module_type * s_with_constraint list
        (* MT with ... *)
  | Mod_typeof of module_expr
        (* module type of ME *)
  | Mod_extension of extension
        (* [%id] *)
  | Mod_alias of Longident.t loc
        (* (module M) *)

and s_module_type =
  {
    mdesc       : s_module_type_desc;
    mloc        : Location.t;
    mattributes : attributes; (* ... [@id1] [@id2] *)
  }

and s_module_declaration =
  {
    mdname       : string loc;
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
