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
  open Ppxlib
  open Identifier
  open Uast

  let mk_loc (s, e) = {
    Location.loc_start = s;
    Location.loc_end = e;
    Location.loc_ghost = false;
  }

  let mk_pid pid l = Preid.create pid ~attrs:[] ~loc:(mk_loc l)
  let mk_term d l = { term_desc = d; term_loc = mk_loc l }
  let mk_pat d l = { pat_desc  = d; pat_loc  = mk_loc l }

  let get_op l = Qpreid (mk_pid (mixfix "[_]") l)
  let set_op l = Qpreid (mk_pid (mixfix "[->]") l)
  let sub_op l = Qpreid (mk_pid (mixfix "[_.._]") l)
  let above_op l = Qpreid (mk_pid (mixfix "[_..]") l)
  let below_op l = Qpreid (mk_pid (mixfix "[.._]") l)

  let id_anonymous loc = Preid.create "_" ~attrs:[] ~loc

  let empty_vspec = {
    sp_header = None;
    sp_pre = [];
    sp_checks = [];
    sp_post = [];
    sp_xpost = [];
    sp_consumes = [];
    sp_produces = [];
    sp_writes = [];
    sp_preserves = [];
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
    fun_coer = false;
    fun_text = "";
    fun_loc = Location.none;
  }

  let loc_of_qualid = function Qpreid pid | Qdot (_, pid) -> pid.pid_loc

  let qualid_preid = function Qpreid p | Qdot (_, p) -> p
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

%token REQUIRES ENSURES VARIANT
%token CONSUMES PRODUCES MODIFIES PRESERVES

(* keywords *)

%token AXIOM
%token EPHEMERAL ELSE EXISTS FALSE FORALL FUNCTION FUN
%token REC
%token INVARIANT
%token COERCION
%token IF IN
%token OLD NOT RAISES
%token THEN TRUE EQUIVALENT CHECKS DIVERGES PURE

%token AS
%token LET MATCH PREDICATE
%token WITH

(* symbols *)

%token LENS
%token AND AMPAMP ARROW BAR BARBAR COLON COLONCOLON COMMA DOT DOTDOT
%token EOF EQUAL
%token MODEL
%token LRARROW LEFTBRC LEFTBRCCOLON LEFTPAR LEFTBRCRIGHTBRC
%token LEFTSQ LTGT OR QUESTION RIGHTBRC COLONRIGHTBRC RIGHTPAR RIGHTSQ SEMICOLON
%token LEFTSQRIGHTSQ
%token STAR TILDE UNDERSCORE
%token WHEN


(* priorities *)

%nonassoc IN
%nonassoc DOT ELSE
%nonassoc prec_named
%right COLON AS

%right ARROW LRARROW
%nonassoc WHEN
%nonassoc RIGHTSQ
%right OR BARBAR
%right AND AMPAMP
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

%left BAR

%start <Uast.function_> func
%start <Uast.axiom> axiom
%start <Uast.val_spec> val_spec
%start <Uast.type_spec> type_spec
%start <Uast.fun_spec> func_spec

%%

val_spec:
| hd=val_spec_header? bd=val_spec_body EOF
  { { bd with sp_header = hd } }
;

axiom:
| AXIOM id=lident COLON t=term EOF
  { {ax_name = id; ax_term = t; ax_loc = mk_loc $loc; ax_text = ""} }
;

func:
| FUNCTION fun_rec=boption(REC) fun_name=func_name fun_params=loption(params)
    COLON ty=typ fun_def=preceded(EQUAL, term)? EOF
  { { fun_name; fun_rec; fun_type = Some ty; fun_params; fun_def; fun_spec = None;
      fun_loc = mk_loc $loc; fun_text = "" } }
| PREDICATE fun_rec=boption(REC) fun_name=func_name fun_params=params
    fun_def=preceded(EQUAL, term)? EOF
  { { fun_name; fun_rec; fun_type = None; fun_params; fun_def; fun_spec = None;
      fun_loc = mk_loc $loc; fun_text = "" } }
;

func_name:
| l=lident_rich { l }
| LEFTPAR LEFTBRCRIGHTBRC RIGHTPAR
  { mk_pid (mixfix "{}") $loc }
| LEFTPAR LEFTBRCCOLON UNDERSCORE COLONRIGHTBRC RIGHTPAR
  { mk_pid (mixfix "{:_:}") $loc }

func_spec:
| EOF { empty_fspec }
| s = nonempty_func_spec EOF { s }

nonempty_func_spec:
| REQUIRES t=term bd=func_spec
  { { bd with fun_req = t :: bd.fun_req } }
| ENSURES t=term bd=func_spec
  { { bd with fun_ens = t :: bd.fun_ens } }
| VARIANT t=term bd=func_spec
  { { bd with fun_variant = t :: bd.fun_variant } }
| COERCION bd=func_spec
  { { bd with fun_coer = true } }
;

type_spec:
| e=ts_ephemeral m=type_spec_model i=ts_invariants EOF
  { { ty_ephemeral = e;
      ty_model = m;
      ty_invariant = i;
      ty_text = "";
      ty_loc = Location.none;
  } }
;

ts_ephemeral:
| EPHEMERAL { true }
|           { false }
;

ts_invariants:
| WITH id=lident l=nonempty_list(ts_invariant) { Some (id, l) }
|                                              { None }
;

ts_invariant:
| INVARIANT inv=term { inv }
;

type_spec_model:
  | (* epsilon *)
    { Self }
  | model = type_spec_model_default
    { Default model }
  |  mod_fields=nonempty_list(type_spec_model_field)
    { Fields mod_fields }
;

type_spec_model_default:
| MODEL COLON f_pty=typ
  { f_pty }
;

type_spec_model_field:
| MODEL f_preid=lident COLON f_pty=typ
  { f_preid, f_pty }
;

val_spec_header:
| ret=ret_name nm=lident_rich args=fun_arg*
  { { sp_hd_nm = nm; sp_hd_ret = ret; sp_hd_args = args } }
| ret=ret_name arg1=fun_arg nm=op_symbol arg2=fun_arg
  { let sp_hd_nm = Preid.create ~loc:(mk_loc $loc(nm)) nm in
    { sp_hd_nm; sp_hd_ret = ret; sp_hd_args = [ arg1; arg2 ] } }
| nm=lident_rich args=fun_arg*
  { { sp_hd_nm = nm; sp_hd_ret = []; sp_hd_args = args } }
;

lens_term:
| t=term { {s_term = t; s_lens = None} }
| t=term LENS ty=typ { {s_term = t; s_lens = Some ty } }
;

val_spec_body:
| (* Empty spec *) { empty_vspec }
| PURE bd=val_spec_body
  { {bd with sp_pure = true} }
| DIVERGES bd=val_spec_body
  { {bd with sp_diverge = true} }
| MODIFIES wr=separated_list(COMMA, lens_term) 
           bd=val_spec_body
  { { bd with sp_writes = wr @ bd.sp_writes } }
| CONSUMES cs=separated_list(COMMA, lens_term) 
           bd=val_spec_body
  { { bd with sp_consumes = cs @ bd.sp_consumes } }
| PRODUCES cs=separated_list(COMMA, lens_term) 
           bd=val_spec_body
  { { bd with sp_produces = cs @ bd.sp_produces } }
| PRESERVES cs=separated_list(COMMA, lens_term) 
           bd=val_spec_body
  { { bd with sp_preserves = cs @ bd.sp_preserves } }
| REQUIRES t=term bd=val_spec_body
  { { bd with sp_pre = t :: bd.sp_pre } }
| CHECKS t=term bd=val_spec_body
  { { bd with sp_checks = t :: bd.sp_checks } }
| ENSURES t=term bd=val_spec_body
  { { bd with sp_post = t :: bd.sp_post} }
| RAISES r=bar_list1(raises) bd=val_spec_body
  { let xp = mk_loc $loc(r), r in
    { bd with sp_xpost = xp :: bd.sp_xpost } }
| EQUIVALENT e=STRING bd=val_spec_body
  { { bd with sp_equiv = e :: bd.sp_equiv} }
;

fun_arg:
| LEFTPAR RIGHTPAR
  { Lunit }
| l=lident
  { Lnone l }
| TILDE l=lident
  { Lnamed l }
| QUESTION l=lident
  { Loptional l }
| LEFTSQ id=lident COLON ty=typ RIGHTSQ
  { Lghost (id, ty) }
;

ret_value:
| l=lident
  { Lnone l }
| LEFTSQ id=lident COLON ty=typ RIGHTSQ
  { Lghost (id, ty) }
;

ret_name:
| LEFTPAR l=separated_list(COMMA, ret_value) RIGHTPAR EQUAL
  { l }
| l=comma_list(ret_value) EQUAL { l } ;

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
| p=param  { p }
| p=param l=params { p @ l }
;

param:
| LEFTPAR params=lident+ COLON t=ty RIGHTPAR
  { List.map (fun x -> mk_loc $loc, x, t) params }
;

cast:
| COLON ty=ty_arg  { ty }
;

term: t = mk_term(term_) { t }
;

term_:
| t=term_arg_
    { t }
| NOT t=term
    { Tnot t }
| OLD t=term
    { Told t }
| op=prefix_op t=term %prec prec_prefix_op
    { Tidapp (Qpreid op, [t]) }
| l = term ; o = bin_op ; r = term
    { Tbinop (l, o, r) }
| l = term ; o = infix_op_1 ; r = term
    { Tinfix (l, o, r) }
| l = term ; o = infix_op_234 ; r = term
    { Tidapp (Qpreid o, [l; r]) }
| l = term ; COLONCOLON ; r = term
    { Tidapp (Qpreid (mk_pid (infix "::") $loc), [l; r]) }
| l = term ; o = BACKQUOTE_LIDENT ; r = term
    { let id = mk_pid o $loc in
      Tidapp (Qpreid id, [l; r]) }
| t=term_arg args=located(term_arg)+
    { let join f (a, _ ,e) = mk_term (Tapply (f, a)) ($startpos, e) in
      (List.fold_left join t args).term_desc }
| IF g=term THEN t1=term ELSE t2=term
    { Tif (g, t1, t2) }
| LET pat=pattern EQUAL t1=term IN t2=term
    { let cast ty = { t1 with term_desc = Tcast (t2, ty) } in
      let pat, def = match pat.pat_desc with
        | Ptuple [] -> { pat with pat_desc = Pwild }, cast (PTtuple [])
        | Pcast ({pat_desc = (Pvar _|Pwild); _} as p, ty) -> p, cast ty
        | _ -> pat, t1 in
      match pat.pat_desc with
      | Pvar id -> Tlet (id, def, t2)
      | Pwild -> Tlet (id_anonymous pat.pat_loc, def, t2)
      | _ -> Tcase (def, [pat, None, t2]) }
| LET op=attrs(lident_op_id) EQUAL t1=term IN t2=term
    { Tlet (op, t1, t2) }
| MATCH t=term WITH cases=match_cases(term)
    { Tcase (t, cases) }
| MATCH t=comma_list2(term) WITH cases=match_cases(term)
    { Tcase (mk_term (Ttuple t) $loc(t), cases) }
| q=quant vars=comma_list1(quant_vars) DOT t=term
    { Tquant (q, List.concat vars, t) }
| FUN args = pat_arg+ ty = preceded(COLON,fun_typ)? ARROW t = term
    { Tlambda (args, t, ty) }
| att=attr t=term %prec prec_named
    { Tattr (att, t) }
| t=term pty=cast
    { Tcast (t, pty) }
;

field_list1(X):
| fl = semicolon_list1(term_rec_field(X)) { fl }
;

term_rec_field(X):
| l=separated_pair(lqualid, EQUAL, X) { l }
| l=lqualid { let t = {term_desc = Tpreid l;
                     term_loc = loc_of_qualid l} in
            (l,t)
          }
;

match_cases(X):
| cl = bar_list1(separated_pair(guarded_pattern, ARROW, X))
    { List.map (fun ((a,g),c) -> a,g,c) cl }
;

guarded_pattern:
| p=pattern { p, None }
| p=pattern WHEN t=term { p, Some t }
;

lens_cast:
| (* epsilon *) { Infer }
| t = cast { Pty t }
| t = cast LENS m = ty_arg { Lens(t, m) }
;

quant_vars:
| vars=binder_var+ pty=lens_cast { List.map (fun id -> id, pty) vars }
;

attrs(X): x=X l=attr* { List.fold_left (fun acc s -> Preid.add_attr acc s) x l }
;

attr:
| attr=ATTRIBUTE    { attr }
;

term_arg: t=mk_term(term_arg_) { t };
term_dot: t=mk_term(term_dot_) { t }
;

term_arg_:
| q=qualid                  { Tpreid q }
| c=constant                { Tconst c }
| TRUE                      { Ttrue }
| FALSE                     { Tfalse }
| o = oppref ; a = term_arg { Tidapp (Qpreid o, [a]) }
| t=term_sub_               { t }
;

term_dot_:
| id=lqualid                { Tpreid id }
| o = oppref ; a = term_dot { Tidapp (Qpreid o, [a]) }
| t=term_sub_               { t }
;

term_block_:
| LEFTPAR t=term RIGHTPAR                           { t.term_desc }
| LEFTPAR RIGHTPAR                                  { Ttuple [] }
| LEFTSQRIGHTSQ
    { Tpreid (Qpreid (mk_pid "[]"  $loc)) }
| LEFTBRC fields=field_list1(term) RIGHTBRC         { Trecord fields }
| LEFTBRCRIGHTBRC
    { Tpreid (Qpreid (mk_pid (mixfix "{}") $loc)) }
| LEFTBRCCOLON t=term COLONRIGHTBRC
    { let id = Qpreid (mk_pid (mixfix "{:_:}") $loc) in
      Tidapp (id, [t]) }
;

location(X): X { $loc($1) };

term_sub_:
| t=term_block_
  { t }
| q=uqualid DOT t=mk_term(term_block_)
  { Tscope (q, t) }
| t=term_dot DOT id=lqualid_rich
  { Tfield (t, id) }
| s=term_arg l=location(LEFTSQ) i=term RIGHTSQ
  { Tidapp (get_op l, [s;i]) }
| m=term_arg l=location(LEFTSQ) t1=term ARROW t2=term RIGHTSQ
  { Tidapp (set_op l, [m;t1;t2]) }
| s=term_arg l=location(LEFTSQ) i1=term DOTDOT i2=term RIGHTSQ
  { Tidapp (sub_op l, [s;i1;i2]) }
| s=term_arg l=location(LEFTSQ) i=term DOTDOT RIGHTSQ
  { Tidapp (above_op l, [s;i]) }
| s=term_arg l=location(LEFTSQ) DOTDOT i=term RIGHTSQ
  { Tidapp (below_op l, [s;i]) }
| LEFTPAR l=comma_list2(term) RIGHTPAR
  { Ttuple l }
;

%inline bin_op:
| ARROW   { Timplies }
| LRARROW { Tiff }
| OR      { Tor }
| BARBAR  { Tor_asym }
| AND     { Tand }
| AMPAMP  { Tand_asym }
;

quant:
| FORALL  { Tforall }
| EXISTS  { Texists }
;

constant:
| n=INTEGER   { let i, m = n in Pconst_integer (i, m) }
| f=FLOAT     { Pconst_float (f, None) }
| s=STRING    { Pconst_string (s, mk_loc $loc, None) }
| c=CHAR      { Pconst_char c }
;

binder_var:
| id=attrs(lident)  { id }
;

mk_expr(X): d = X { mk_expr d $loc }
;

typ:
| ty=ty_arg
    { ty }
| id=lident COLON aty=typ ARROW rty=typ
    { PTarrow (Lnamed id, aty, rty) }
| QUESTION id=lident COLON aty=typ ARROW rty=typ
    { PTarrow (Loptional id, aty, rty) }
| ty1=typ ARROW ty2=typ
    { let l = mk_loc $loc in
      PTarrow (Lnone (id_anonymous l), ty1, ty2) }
| ty=ty_arg STAR l=ty_tuple
    { PTtuple (ty :: l) }
;

fun_typ:
| ty=ty_arg
    { ty }
| ty=ty_arg STAR l=ty_tuple
    { PTtuple (ty :: l) }
;

ty_tuple:
| ty=ty_arg
    { [ty] }
| ty=ty_arg STAR l=ty_tuple
    { ty :: l }
;

ty_arg:
| q=lqualid
    { PTtyapp (q, []) }
| id=quote_lident
    { PTtyvar id }
| LEFTPAR ty=typ RIGHTPAR
    { ty }
| ty=ty_arg id=lqualid
    { PTtyapp (id, [ty]) }
| LEFTPAR ty=typ COMMA l=separated_nonempty_list(COMMA, typ)  RIGHTPAR id=lqualid
    { PTtyapp (id, ty::l) }
;

%inline ty:
| ty=typ {ty}
;

mk_term(X): d = X { mk_term d $loc }
;

(* Patterns *)

mk_pat(X): p=X { mk_pat p $loc }
;

pattern: p=mk_pat(pattern_) { p };
pat_arg: p=mk_pat(pat_arg_) { p }
pat_arg_no_lpar: p=mk_pat(pat_arg_no_lpar_) { p }
;

pattern_:
| p=pat_conj_                           { p }
| p1=mk_pat(pat_conj_) BAR p2=pattern   { Por (p1,p2) }
;

pat_conj_:
| p=pat_uni_                            { p }
| l=comma_list2(mk_pat(pat_uni_))       { Ptuple l }
;

pat_uni_:
| p=pat_arg_
  { p }
| ph=pat_arg COLONCOLON pt=mk_pat(pat_uni_)
  { Papp (Qpreid (mk_pid (infix "::") $loc),[ph;pt]) }
| q=uqualid LEFTPAR l=separated_list(COMMA, mk_pat(pat_uni_)) RIGHTPAR
  { Papp (q,l) }
| q=uqualid l=pat_arg_no_lpar
  { Papp (q,[l]) }
| p=mk_pat(pat_uni_) AS id=attrs(lident)
  { Pas (p,id) }
| p=mk_pat(pat_uni_) ty=cast
  { Pcast (p, ty) }
;

pat_arg_:
| LEFTPAR RIGHTPAR                      { Ptuple [] }
| LEFTPAR p=pattern_ RIGHTPAR           { p }
| p=pat_arg_no_lpar_                    { p }
;

pat_arg_no_lpar_:
| id=attrs(lident)                      { Pvar id }
| UNDERSCORE                            { Pwild }
| q=uqualid                             { Papp (q,[]) }
| c=constant                            { Pconst c }
| c1=CHAR DOTDOT c2=CHAR                { Pinterval (c1,c2) }
| TRUE                                  { Ptrue }
| FALSE                                 { Pfalse }
| LEFTSQRIGHTSQ
  { Papp (Qpreid (mk_pid "[]"  $loc), []) }
| LEFTBRC p=field_pattern(pattern) RIGHTBRC 
  { Prec p }
;



field_pattern(X):
| fl = semicolon_list1(pattern_rec_field(X)) { fl }
;

pattern_rec_field(X):
| p=separated_pair(lqualid, EQUAL, X) { p }
| q=lqualid { let p = {pat_desc = Pvar (qualid_preid q);
                     pat_loc = loc_of_qualid q} in
            (q,p)
          }
;

(* Symbolic operation names *)

op_symbol:
| o=OP1  { o }
| o=OP2  { o }
| o=OP3  { o }
| o=OP4  { o }
| STAR { "*" }
;

%inline oppref:
| o = OPPREF { mk_pid (prefix o) $loc }
;

prefix_op:
| o=op_symbol { mk_pid (prefix o) $loc }
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
;

(* Idents *)

lident:
| id=LIDENT        { mk_pid id $loc }
;

uident:
| id=UIDENT        { mk_pid id $loc }
;

quote_lident:
| id=QUOTE_LIDENT  { mk_pid id $loc }
;

ident_rich:
| id=uident        { id }
| id=lident_rich   { id }
;

lident_rich:
| id=lident        { id }
| id=lident_op_id  { id }
;

lident_op_id:
| LEFTPAR op=lident_op RIGHTPAR  { mk_pid op $loc }
;

lident_op:
| op=op_symbol                                { infix op }
| op=op_symbol UNDERSCORE                     { prefix op }
| EQUAL                                       { infix "=" }
| op=OPPREF UNDERSCORE?                       { prefix op }
| DOT LEFTPAR RIGHTPAR                        { mixfix ".()" }
| LEFTSQ UNDERSCORE RIGHTSQ                   { mixfix "[_]" }
| LEFTSQ ARROW RIGHTSQ                        { mixfix "[->]" }
| LEFTSQ UNDERSCORE DOTDOT UNDERSCORE RIGHTSQ { mixfix "[_.._]" }
| LEFTSQ            DOTDOT UNDERSCORE RIGHTSQ { mixfix "[.._]" }
| LEFTSQ UNDERSCORE DOTDOT            RIGHTSQ { mixfix "[_..]" }
;

(* Qualified idents *)

qualid:
| id=ident_rich                { Qpreid id }
| q=uqualid DOT id=ident_rich  { Qdot (q, id) }
;

lqualid_rich:
| id=lident_rich               { Qpreid id }
| q=uqualid DOT id=lident_rich { Qdot (q, id) }
;

lqualid:
| id=lident                    { Qpreid id }
| q=uqualid DOT id=lident      { Qdot (q, id) }
;

uqualid:
| id=uident                    { Qpreid id }
| q=uqualid DOT id=uident      { Qdot (q, id) }
;

(* Miscellaneous *)

comma_list(X):
| l=separated_nonempty_list(COMMA, X) { l }
;

bar_list1(X):
| ioption(BAR) ; xl = separated_nonempty_list(BAR, X) { xl }
;

with_list1(X):
| l=separated_nonempty_list(WITH, X)  { l }
;

comma_list2(X):
| x=X COMMA l=comma_list1(X) { x :: l }
;

comma_list1(X):
| l=separated_nonempty_list(COMMA, X) { l }
;

semicolon_list1(X):
| x = X ; ioption(SEMICOLON)                  { [x] }
| x = X ; SEMICOLON ; xl = semicolon_list1(X) { x :: xl }
;

star_list2(X):
| x=X STAR l=separated_nonempty_list(STAR, X) { x :: l }
;

located(X): x=X { x, $startpos, $endpos }
;
