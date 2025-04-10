(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

%{
  module W = Warnings
  open Ppxlib
  open Preid
  open Parse_uast
  open Uast_utils

  let mk_pid pid l = Preid.create pid ~attrs:[] ~loc:(mk_loc l)
  let mk_term d l = { term_desc = d; term_loc = mk_loc l }
  let mk_pat d l = { pat_desc  = d; pat_loc  = mk_loc l }

  let get_op l = mk_pid (mixfix "[_]") l
  let set_op l = mk_pid (mixfix "[->]") l
  let sub_op l = mk_pid (mixfix "[_.._]") l
  let above_op l = mk_pid (mixfix "[_..]") l
  let below_op l = mk_pid (mixfix "[.._]") l

  let id_anonymous loc = Preid.create "_" ~attrs:[] ~loc

  let empty_vspec = {
    sp_header = None;
    sp_pre = [];
    sp_checks = [];
    sp_post = [];
    sp_xpost = [];
    sp_writes = [];
    sp_consumes= [];
    sp_diverge = false;
    sp_pure = false;
    sp_equiv = [];
    sp_text = "";
    sp_loc = Location.none;
  }

  let empty_fspec = {
    fun_req = [];
    fun_ens = [];
    fun_variant = [];
    fun_text = "";
    fun_loc = Location.none;
  }

  let loc_of_qualid = function Qid pid | Qdot (_, pid) -> pid.pid_loc

  let qualid_preid = function Qid p | Qdot (_, p) -> p

  let combine_spec s1 s2 =
    {
      s1 with sp_post = s2.sp_post;
              sp_xpost = s2.sp_xpost;
	      sp_equiv = s2.sp_equiv
    }
%}

(* Tokens *)

%token <string> LIDENT UIDENT
%token <string> OP1 OP2 OP3 OP4 OPPREF
%token <string> QUOTE_LIDENT
%token <string> BACKQUOTE_LIDENT
%token <string> ATTRIBUTE

%token <string * char option> INTEGER
%token <string> FLOAT
%token <char> CHAR
%token <string> STRING

(* Spec Tokens *)

%token REQUIRES ENSURES CONSUMES VARIANT

(* keywords *)

%token AXIOM
%token MUTABLE MODEL ELSE EXISTS FALSE FORALL FUNCTION FUN
%token REC AND
%token INVARIANT
%token IF IN
%token OLD NOT RAISES
%token THEN TRUE MODIFIES EQUIVALENT CHECKS DIVERGES PURE
%token TRUEPROP FALSEPROP
%token OPEN

%token AS
%token LET PREDICATE
%token WITH
%token TYPE

(* symbols *)

%token CONJ AMPAMP ARROW BAR BARBAR COLON COLONCOLON COMMA DOT DOTDOT
%token EOF EQUAL
%token LRARROW LEFTBRC LEFTBRCCOLON LEFTPAR LEFTBRCRIGHTBRC
%token LEFTSQ LTGT DISJ QUESTION RIGHTBRC COLONRIGHTBRC RIGHTPAR RIGHTSQ SEMICOLON
%token LEFTSQRIGHTSQ
%token STAR TILDE UNDERSCORE
(* priorities *)

%nonassoc IN
%nonassoc DOT ELSE
%nonassoc prec_named
%right COLON AS

%right ARROW LRARROW
%nonassoc RIGHTSQ
%right DISJ BARBAR
%right CONJ AMPAMP
%nonassoc NOT
%right EQUAL LTGT OP1
%right COLONCOLON
%nonassoc OLD
%left OP2
%left OP3 STAR
%left OP4
%left BACKQUOTE_LIDENT
%nonassoc prec_prefix_op
%nonassoc LEFTSQ
%nonassoc OPPREF

%start <Parse_uast.gospel_signature> top
%start <Parse_uast.val_spec> val_spec
%start <Parse_uast.type_spec> type_spec
%%

top: | s = top_ EOF { s }

top_:
| f = func
  { Sig_function f }
| ax = axiom
  { Sig_axiom ax }
| tdecl = type_decl(TYPE)
  { Sig_ghost_type tdecl }
| OPEN q=uqualid
  { Sig_ghost_open q }

decl_params:
| (* epsilon *)
  { [] }
| id=quote_lident
  { [id] }
| LEFTPAR l=separated_nonempty_list(COMMA, quote_lident) RIGHTPAR
  { l }

record_field:
| id=lident COLON t=typ
    { { pld_name = id;
	pld_mutable = Immutable;
	pld_type = t;
	pld_loc = mk_loc $loc } }

type_def:
| t=typ
  { PTtype_abstract, Some t }
| LEFTBRC l = semicolon_list1(record_field) RIGHTBRC
  { PTtype_record l, None }

type_decl_rec:
| (* espilon *)      { [] }
| l = type_decl(AND) { l }

type_decl(X):
| X tparams=decl_params id=lident def=preceded(EQUAL, type_def)?
       inv=ts_invariants l=located_begin(type_decl_rec)
  {
    let kind, alias =
      Option.value ~default:(PTtype_abstract, None) def in
    let l, end_pos = l in
    let begin_pos, _ = $loc in
    {
      tname = id;
      tparams = tparams;
      tkind = kind;
      tmanifest = alias;
      tprivate = Public;
      tattributes = [];
      tspec = Some {
		  ty_mutable = false;
		  ty_invariant = inv;
		  ty_model = No_model;
		  ty_text = "";
		  ty_loc = mk_loc $loc(inv);
		};
      tloc = mk_loc (begin_pos, end_pos);
  } :: l
}

val_spec:
| h=val_spec_header IN s=val_spec_post EOF
  { { s with sp_header= Some h } }
| s1=val_spec_pre h=val_spec_header IN s2=val_spec_post EOF
  { { (combine_spec s1 s2) with sp_header = Some h } }
| s=val_spec_pre h=val_spec_header? EOF
  { { s with sp_header = h } }
| s=val_spec_post_empty EOF
  { s }
;

axiom:
| AXIOM id=lident COLON t=term
  { {ax_name = id; ax_term = t; ax_loc = mk_loc $loc; ax_text = ""} }
;

func_(X, PTY):
| X fun_rec=boption(REC) fun_name=func_name fun_params=loption(params)
    ty=PTY fun_def=preceded(EQUAL, term)? spec=func_spec
  { { fun_name; fun_rec; fun_type = ty; fun_params; fun_def; fun_spec = spec;
      fun_loc = mk_loc $loc; } }

ftyp: COLON t=typ { Some t }

epsilon: { None }

func:
| f=func_(FUNCTION, ftyp) { f }
| f=func_(PREDICATE, epsilon) { f }

func_name:
| id = lident { id }
| id = uident { id }
| id = lident_fun_id { id }
| NOT { mk_pid "not" $loc }
| LEFTPAR LEFTBRCRIGHTBRC RIGHTPAR
  { mk_pid (mixfix "{}") $loc }
| LEFTPAR LEFTBRCCOLON UNDERSCORE COLONRIGHTBRC RIGHTPAR
  { mk_pid (mixfix "{:_:}") $loc }

func_spec:
| (* epsilon*)              { empty_fspec }
| spec = nonempty_func_spec { spec }

nonempty_func_spec:
| REQUIRES t=term bd=func_spec
  { { bd with fun_req = t :: bd.fun_req } }
| ENSURES t=term bd=func_spec
  { { bd with fun_ens = t :: bd.fun_ens } }
| VARIANT t=term bd=func_spec
  { { bd with fun_variant = t :: bd.fun_variant } }
;

model_field:
| MODEL id = lident COLON t = typ
  { id, t }

ts_model:
| (* epsilon *)
  { No_model }
| MODEL COLON t=typ
  { Implicit t }
| l = model_field+
  { Fields l }

type_spec:
| m=ts_mutable model=ts_model i=ts_invariants EOF
  { { ty_mutable = m;
      ty_invariant = i;
      ty_model = model;
      ty_text = "";
      ty_loc = Location.none;
  } }
;

ts_mutable:
| MUTABLE { true }
|         { false }
;

ts_invariants:
| WITH id=lident l=nonempty_list(ts_invariant) { Some (id, l) }
|                                              { None }
;

ts_invariant:
| INVARIANT inv=term { inv }
;

val_spec_own:
| l=separated_nonempty_list(COMMA, term_dot)
   { l }

val_spec_pre:
| MODIFIES wr=val_spec_own bd=val_spec_pre_empty
  { { bd with sp_writes = wr @ bd.sp_writes } }
| CONSUMES cs=val_spec_own bd=val_spec_pre_empty
  { { bd with sp_consumes = cs @ bd.sp_consumes } }
| REQUIRES t=term bd=val_spec_pre_empty
  { { bd with sp_pre = t :: bd.sp_pre } }
| CHECKS t=term bd=val_spec_pre_empty
  { { bd with sp_checks = t :: bd.sp_checks } }
| PURE bd=val_spec_pre_empty
  { { bd with sp_pure = true } }
| DIVERGES bd=val_spec_pre_empty
  { { bd with sp_diverge = true } }
;

val_spec_pre_empty:
| (* epsilon *)
  { empty_vspec }
| bd=val_spec_pre
  { bd }

val_spec_header:
| LET ret=ret_name nm=lident_rich args=fun_arg+
  { { sp_hd_nm = nm; sp_hd_ret = ret; sp_hd_args = args } }
| LET ret=ret_name arg1=fun_arg nm=op_symbol arg2=fun_arg
  { let sp_hd_nm = Preid.create ~loc:(mk_loc $loc(nm)) nm in
    { sp_hd_nm; sp_hd_ret = ret; sp_hd_args = [ arg1; arg2 ] } }
;

val_spec_post:
| ENSURES t=term bd=val_spec_post_empty
  { { bd with sp_post = t :: bd.sp_post} }
| RAISES r=bar_list1(raises) bd=val_spec_post_empty
  { let xp = mk_loc $loc(r), r in
    { bd with sp_xpost = xp :: bd.sp_xpost } }
| EQUIVALENT e=STRING bd=val_spec_post_empty
  { { bd with sp_equiv = e :: bd.sp_equiv } }

val_spec_post_empty:
| (* epsilon *) { empty_vspec }
| bd=val_spec_post { bd }

fun_arg:
| LEFTPAR RIGHTPAR
  { Lunit }
| id = lident
  { Lnone id }
| TILDE id = lident
  { Lnamed id }
| QUESTION id = lident
  { Loptional id }
| LEFTSQ id=lident COLON ty=typ RIGHTSQ
  { Lghost (id, ty) }
;

ret_value:
| id = lident
  { Lnone id }
| LEFTSQ id=lident COLON ty=typ RIGHTSQ
  { Lghost (id, ty) }
;

ret_name:
| LEFTPAR l=comma_list(ret_value) RIGHTPAR EQUAL
  { l }
| l=comma_list(ret_value) EQUAL { l }
| UNDERSCORE EQUAL { [] };

raises:
| q=uqualid ARROW t=term
  { q, Some (mk_pat (Ptuple []) $loc(q), t) }
| q=uqualid p=pat_arg ARROW t=term
  { q, Some (p, t) }
| q=uqualid p=pat_arg
  { q, Some (p, mk_term Ttrue $loc(p)) }
| q=uqualid
  { q, None}
;

params:
| p = param  { p }
| p = param l = params { p @ l }
;

param:
| LEFTPAR params=lident+ COLON t=ty RIGHTPAR
  { List.map (fun x -> x, t) params }
;

cast:
| COLON ty = ty_arg  { ty }
;

term: t = mk_term(term_) { t }
;

term_:
| term_arg_
    { chain_desc $1 }
| NOT t = term
    { mk_op_apply (mk_pid "not" $loc($1)) [t] }
| OLD term
    { Told $2 }
| op = prefix_op t = term %prec prec_prefix_op
    { mk_op_apply op [t] }
| l = term ; o = infix_op_1 ; r = term
    { Tinfix (l, o, r) }
| l = term ; o = infix_op_234 ; r = term
    { mk_op_apply o [l; r] }
| l = term ; COLONCOLON ; r = term
    { mk_op_apply (mk_pid (infix "::") $loc) [l; r] }
| l = term ; o = BACKQUOTE_LIDENT ; r = term
    { mk_op_apply (mk_pid o $loc) [l; r] }
| term_arg located(term_arg)+
    { let join f (a, e) = mk_term (Tapply (f, a)) ($startpos, e) in
      (List.fold_left join $1 $2).term_desc }
| IF g = term THEN t1 = term ELSE t2 = term
    { Tif (g, t1, t2) }
| LET p = pattern EQUAL t1 = term IN t2 = term
    { let cast ty = { t1 with term_desc = Tcast (t1, ty) } in
      let pat, def = match p.pat_desc with
        | Ptuple [] -> { p with pat_desc = Pwild }, cast (PTtuple [])
        | Pcast ({pat_desc = (Pvar _|Pwild); _} as p, ty) -> p, cast ty
        | _ -> p, t1 in
      match pat.pat_desc with
      | Pvar id -> Tlet (id, def, t2)
      | Pwild -> Tlet (id_anonymous pat.pat_loc, def, t2)
      | _ -> W.error ~loc:pat.pat_loc W.Syntax_error  }
| LET id = attrs(lident_op_id) EQUAL t1 = term IN t2 = term
    { Tlet (id, t1, t2) }
| q = quant l = comma_list1(quant_vars) DOT t = term
    { Tquant (q, List.concat l, t) }
| FUN args = fun_vars+ ty = preceded(COLON,fun_typ)? ARROW t = term
    { Tlambda (args, t, ty) }
| a = attr t = term %prec prec_named
    { Tattr (a, t) }
| t = term ty = cast
    { Tcast (t, ty) }
;

field_list1(X):
| fl = semicolon_list1(term_rec_field(X)) { fl }
;

term_rec_field(X):
| l = separated_pair(lqualid, EQUAL, X) { l }
| q = lqualid { let t = {term_desc = Tvar q;
                     term_loc = loc_of_qualid q} in
            (q,t)
          }
;

fun_vars:
| v=binder_var { v, None }
| LEFTPAR v=binder_var t=cast RIGHTPAR { v, Some t }

quant_vars:
| l = binder_var+ ty = cast? { List.map (fun id -> id, ty) l }
;

attrs(X): l=X a=attr* { List.fold_left (fun acc s -> Preid.add_attr acc s) l a }
;

attr:
| a = ATTRIBUTE    { a }
;

term_arg: t = mk_term(term_arg_) { t };
term_dot: t = mk_term(term_dot_) { t }
;

term_arg_:
| q = qualid                { Tvar q }
| c = constant              { Tconst c }
| TRUE                      { Ttrue }
| FALSE                     { Tfalse }
| TRUEPROP                  { TTrue }
| FALSEPROP                 { TFalse }
| o = oppref ; a = term_arg { mk_op_apply o [a] }
| t = term_sub_             { t }
;

term_dot_:
| q = lqualid               { Tvar q }
| o = oppref ; a = term_dot { mk_op_apply o [a] }
| t = term_sub_             { t }
;

term_block_:
| LEFTPAR t=term RIGHTPAR                           { t.term_desc }
| LEFTPAR RIGHTPAR                                  { Ttuple [] }
| LEFTSQRIGHTSQ
    { Tvar (Qid (mk_pid "[]"  $loc)) }
| LEFTBRC r = field_list1(term) RIGHTBRC            { Trecord r }
| LEFTBRCRIGHTBRC
    { Tvar (Qid (mk_pid (mixfix "{}") $loc)) }
| LEFTBRCCOLON t=term COLONRIGHTBRC
    { let id = mk_pid (mixfix "{:_:}") $loc in
      mk_op_apply id [t] }
;

term_sub_:
| t = term_block_
    { t }
| q = uqualid DOT t = mk_term(term_block_)
    { Tscope (q, t) }
| t = term_dot DOT q = lqualid_rich
    { Tfield (t, q) }
| s = term_arg l = location(LEFTSQ) i = term RIGHTSQ
    { mk_op_apply (get_op l) [s; i] }
| m = term_arg l = location(LEFTSQ) k = term ARROW v = term RIGHTSQ
    { mk_op_apply (set_op l) [m; k; v] }
| s = term_arg l = location(LEFTSQ) i1 = term DOTDOT i2 = term RIGHTSQ
    { mk_op_apply (sub_op l) [s; i1; i2] }
| s = term_arg l = location(LEFTSQ) i = term DOTDOT RIGHTSQ
    { mk_op_apply (above_op l) [s; i] }
| s = term_arg l = location(LEFTSQ) DOTDOT i = term RIGHTSQ
    { mk_op_apply (below_op l) [s; i] }
| LEFTPAR l = comma_list2(term) RIGHTPAR                { Ttuple l }
;

quant:
| FORALL  { Tforall }
| EXISTS  { Texists }
;

constant:
| v = INTEGER { let i, m = v in Pconst_integer (i, m) }
| v = FLOAT { Pconst_float (v, None) }
| s = STRING { Pconst_string (s, mk_loc $loc, None) }
| c = CHAR { Pconst_char c }
;

binder_var:
| l = attrs(lident)  { l }
;

mk_expr(X): d = X { mk_expr d $loc }
;

typ:
| v = ty_arg
    { v }
| arg = typ ARROW ret = typ
    { PTarrow (arg, ret) }
| t = ty_arg STAR l = ty_tuple
    { PTtuple (t :: l) }
;

fun_typ:
| ty = ty_arg
    { ty }
| ty = ty_arg STAR l = ty_tuple
    { PTtuple (ty :: l) }
;

ty_tuple:
| arg = ty_arg
    { [arg] }
| ty = ty_arg STAR l = ty_tuple
    { ty :: l }
;

ty_arg:
| q = lqualid
    { PTtyapp (q, []) }
| v = quote_lident
    { PTtyvar v }
| LEFTPAR ty = typ RIGHTPAR
    { ty }
| ty = ty_arg q = lqualid
    { PTtyapp (q, [ty]) }
| LEFTPAR ty = typ COMMA l = separated_nonempty_list(COMMA, typ)  RIGHTPAR id=lqualid
    { PTtyapp (id, ty :: l) }
;

%inline ty:
| ty = typ { ty }
;

mk_term(X): d = X { mk_term d $loc }
;

(* Patterns *)

mk_pat(X): t = X { mk_pat t $loc }
;

pattern: p = mk_pat(pattern_) { p };
pat_arg: p = mk_pat(pat_arg_) { p }
pat_arg_no_lpar: p = mk_pat(pat_arg_no_lpar_) { p }
;

pattern_:
| p = pat_conj_                                 { p }
| p1 = mk_pat(pat_conj_) BAR p2 = pattern         { Por (p1, p2) }
;

pat_conj_:
| p = pat_uni_                            { p }
| l = comma_list2(mk_pat(pat_uni_))       { Ptuple l }
;

pat_uni_:
| p = pat_arg_
    { p }
| ph = pat_arg COLONCOLON pt = mk_pat(pat_uni_)
    { Papp (Qid (mk_pid (infix "::") $loc),[ph;pt]) }
| q = uqualid LEFTPAR l = separated_list(COMMA, mk_pat(pat_uni_)) RIGHTPAR
    { Papp (q, l) }
| q = uqualid p = pat_arg_no_lpar
    { Papp (q, [p]) }
| p = mk_pat(pat_uni_) AS id = attrs(lident)
    { Pas (p, id) }
| p = mk_pat(pat_uni_) ty = cast
    { Pcast (p, ty) }
;

pat_arg_:
| LEFTPAR RIGHTPAR                      { Ptuple [] }
| LEFTPAR p = pattern_ RIGHTPAR         { p }
| p = pat_arg_no_lpar_                  { p }
;

pat_arg_no_lpar_:
| id = attrs(lident)                    { Pvar id }
| UNDERSCORE                            { Pwild }
| q = uqualid                           { Papp (q,[]) }
| c = constant                          { Pconst c }
| c1 = CHAR DOTDOT c2 = CHAR            { Pinterval (c1, c2) }
| TRUE                                  { Ptrue }
| FALSE                                 { Pfalse }
| LEFTSQRIGHTSQ
  { Papp (Qid (mk_pid "[]"  $loc), []) }
| LEFTBRC p = field_pattern(pattern) RIGHTBRC { Prec p }
;



field_pattern(X):
| fl = semicolon_list1(pattern_rec_field(X)) { fl }
;

pattern_rec_field(X):
| l = separated_pair(lqualid, EQUAL, X) { l }
| q = lqualid { let p = {pat_desc = Pvar (qualid_preid q);
                     pat_loc = loc_of_qualid q} in
            (q, p)
          }
;

(* Symbolic operation names *)

op_symbol:
| o = OP1 { o }
| o = OP2 { o }
| o = OP3 { o }
| o = OP4 { o }
| ARROW   { "->" }
| LRARROW { "<->" }
| BARBAR  { "||" }
| AMPAMP  { "&&" }
| STAR    { "*"  }
;

%inline oppref:
| o = OPPREF { mk_pid (prefix o) $loc }
;

prefix_op:
| o = op_symbol { mk_pid (prefix o) $loc }
;

%inline infix_op_1:
| o = OP1   { mk_pid (infix o) $loc }
| EQUAL     { mk_pid (infix "=") $loc }
| LTGT      { mk_pid (infix "<>") $loc }
%inline infix_op_234:
| o = OP2   { mk_pid (infix o) $loc }
| o = OP3   { mk_pid (infix o) $loc }
| STAR      { mk_pid (infix "*") $loc }
| o = OP4   { mk_pid (infix o) $loc }
| ARROW     { mk_pid (infix "->")  $loc }
| LRARROW   { mk_pid (infix "<->") $loc }
| DISJ      { mk_pid (infix "\\/") $loc }
| BARBAR    { mk_pid (infix "||")  $loc }
| CONJ      { mk_pid (infix "/\\") $loc }
| AMPAMP    { mk_pid (infix "&&")  $loc }
;

(* Idents *)

lident:
| id = LIDENT        { mk_pid id $loc }
;

uident:
| id = UIDENT        { mk_pid id $loc }
;

quote_lident:
| id = QUOTE_LIDENT  { mk_pid id $loc }
;

ident_rich:
| id = uident        { id }
| id = lident_rich   { id }
;

lident_rich:
| id = lident        { id }
| id = lident_op_id  { id }
;

lident_fun_id:
| LEFTPAR id = lident_op RIGHTPAR  { mk_pid id $loc }

lident_op_id:
| id = lident_fun_id               { id }
| LEFTPAR DISJ RIGHTPAR              { mk_pid (infix "\\/") $loc }
| LEFTPAR CONJ RIGHTPAR             { mk_pid (infix "/\\") $loc }
;

lident_op:
| op = op_symbol                              { infix op }
| op = op_symbol UNDERSCORE                   { prefix op }
| EQUAL                                       { infix "=" }
| LTGT                                        { infix "<>" }
| op = OPPREF UNDERSCORE?                     { prefix op }
| DOT LEFTPAR RIGHTPAR                        { mixfix ".()" }
| LEFTSQ UNDERSCORE RIGHTSQ                   { mixfix "[_]" }
| LEFTSQ ARROW RIGHTSQ                        { mixfix "[->]" }
| LEFTSQ UNDERSCORE DOTDOT UNDERSCORE RIGHTSQ { mixfix "[_.._]" }
| LEFTSQ            DOTDOT UNDERSCORE RIGHTSQ { mixfix "[.._]" }
| LEFTSQ UNDERSCORE DOTDOT            RIGHTSQ { mixfix "[_..]" }
;

(* Qualified idents *)

qualid:
| id = ident_rich                  { Qid id }
| q = uqualid DOT id = ident_rich  { Qdot (q, id) }
;

lqualid_rich:
| id = lident_rich                 { Qid id }
| q = uqualid DOT id = lident_rich { Qdot (q, id) }
;

lqualid:
| id = lident                      { Qid id }
| q = uqualid DOT id = lident      { Qdot (q, id) }
;

uqualid:
| id = uident                      { Qid id }
| q = uqualid DOT id = uident      { Qdot (q, id) }
;

(* Miscellaneous *)

comma_list(X):
| l = separated_nonempty_list(COMMA, X) { l }
;

bar_list1(X):
| ioption(BAR) ; xl = separated_nonempty_list(BAR, X) { xl }
;

with_list1(X):
| l = separated_nonempty_list(WITH, X)  { l }
;

comma_list2(X):
| hd = X COMMA l = comma_list1(X) { hd :: l }
;

comma_list1(X):
| l = separated_nonempty_list(COMMA, X) { l }
;

semicolon_list1(X):
| x = X ; ioption(SEMICOLON)                  { [x] }
| x = X ; SEMICOLON ; xl = semicolon_list1(X) { x :: xl }
;

star_list2(X):
| x = X STAR l = separated_nonempty_list(STAR, X) { x :: l }
;

located(X): x = X { x, $endpos }

located_begin(X) : x = X { x, $startpos }

location(X) : X { $loc($1) }
;
