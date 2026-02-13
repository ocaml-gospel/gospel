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
  open Preid
  open Parse_uast
  open Uast_utils

  let mk_pid pid ?(fixity = Normal) l = Preid.create pid ~fixity ~attrs:[] ~loc:(mk_loc l)
  let mk_term d l = { term_desc = d; term_loc = mk_loc l }
  let mk_pat p l = { pat_desc = p; pat_loc = mk_loc l }

  let get_op l = mk_pid ~fixity:Preid.Mixfix "[_]" l
  let set_op l = mk_pid ~fixity:Preid.Mixfix "[->]" l
  let sub_op l = mk_pid ~fixity:Preid.Mixfix "[_.._]" l
  let above_op l = mk_pid ~fixity:Preid.Mixfix "[_..]" l
  let below_op l = mk_pid ~fixity:Preid.Mixfix "[.._]" l

  let empty_pre_vspec = {
    sp_pre = [];
    sp_checks = [];
    sp_modifies = [];
    sp_preserves = [];
    sp_consumes = [];
    sp_diverge = false;
    sp_pure = false;
  }

  let empty_post_vspec = {
    sp_post = [];
    sp_produces = [];
  }

  let empty_fspec = {
    fun_req = [];
    fun_ens = [];
    fun_variant = [];
    fun_text = "";
    fun_loc = Location.none;
  }

  let loc_of_qualid = function Qid pid | Qdot (_, pid) -> pid.pid_loc

 let mk_spec header pre post xpost =
    {
      sp_header = header;
      sp_pre_spec = pre;
      sp_post_spec = post;
      sp_xpost_spec = xpost;
      sp_text = "";
      sp_loc = Location.none
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

%token REQUIRES ENSURES CONSUMES PRODUCES PRESERVES VARIANT CHECKS

(* keywords *)

%token AXIOM
%token MUTABLE MODEL ELSE EXISTS FALSE FORALL FUNCTION FUN
%token REC AND
%token INVARIANT
%token IF IN
%token OLD NOT
%token THEN TRUE MODIFIES DIVERGES PURE
%token TRUEPROP FALSEPROP
%token OPEN

%token LET PREDICATE
%token WITH
%token TYPE RAISES

(* symbols *)

%token CONJ AMPAMP ARROW BAR BARBAR COLON COLONCOLON COMMA DOT DOTDOT
%token EOF EQUAL
%token LRARROW LEFTBRC LEFTBRCCOLON LEFTPAR LEFTBRCRIGHTBRC
%token LEFTSQ LTGT DISJ RIGHTBRC COLONRIGHTBRC RIGHTPAR RIGHTSQ SEMICOLON
%token LEFTSQRIGHTSQ
%token STAR UNDERSCORE
(* priorities *)

%nonassoc IN
%nonassoc DOT ELSE
%nonassoc prec_named
%right COLON

%right ARROW LRARROW
%nonassoc RIGHTSQ
%right DISJ BARBAR
%right CONJ AMPAMP
%nonassoc NOT
%right EQUAL LTGT OP1
%right COLONCOLON
%left OP2
%left OP3 STAR
%left OP4
%left BACKQUOTE_LIDENT
%nonassoc prec_prefix_op
%nonassoc LEFTSQ
%nonassoc OPPREF
%nonassoc OLD

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
| post=val_spec_post EOF
    { mk_spec None empty_pre_vspec post [] }
| pre=val_spec_pre EOF
   { mk_spec None pre empty_post_vspec [] }
| h=val_spec_header pre=val_spec_pre_empty post=val_spec_post_empty exn=val_spec_exn_empty EOF
  { mk_spec (Some h) pre post exn }
;

val_spec_exn_empty:
| (* epsilon *)
  { [] }
| RAISES e = uqualid xpost_list = val_spec_exn_empty
  { let xpost =
      { sp_exn = e;
        sp_xrets = [];
        sp_xpost = [];
        sp_xproduces = [];
        sp_xloc = mk_loc $loc } in
    xpost :: xpost_list }
| RAISES e = uqualid r = ret_pat
    post = val_spec_post xpost_list = val_spec_exn_empty
  { let xpost =
      { sp_exn = e;
        sp_xrets = r;
        sp_xpost = post.sp_post;
        sp_xproduces = post.sp_produces;
        sp_xloc = mk_loc $loc } in
    xpost :: xpost_list }

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
  { mk_pid ~fixity:Mixfix "{}" $loc }
| LEFTPAR LEFTBRCCOLON UNDERSCORE COLONRIGHTBRC RIGHTPAR
  { mk_pid ~fixity:Mixfix "{:_:}" $loc }

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
| l=separated_nonempty_list(COMMA, qualid)
   { l }

val_spec_pre:
| MODIFIES wr=val_spec_own bd=val_spec_pre_empty
  { { bd with sp_modifies = wr @ bd.sp_modifies } }
| PRESERVES pr=val_spec_own bd=val_spec_pre_empty
  { { bd with sp_preserves = pr @ bd.sp_preserves } }
| CONSUMES cs=val_spec_own bd=val_spec_pre_empty
  { { bd with sp_consumes = cs @ bd.sp_consumes } }
| REQUIRES t=term bd=val_spec_pre_empty
  { { bd with sp_pre = t :: bd.sp_pre } }
| CHECKS t=term bd=val_spec_pre_empty
  { { bd with sp_pre = t :: bd.sp_checks } }
| PURE bd=val_spec_pre_empty
  { { bd with sp_pure = true } }
| DIVERGES bd=val_spec_pre_empty
  { { bd with sp_diverge = true } }
;

val_spec_pre_empty:
| (* epsilon *)
  { empty_pre_vspec }
| bd=val_spec_pre
  { bd }

val_spec_app:
| nm=lident_rich args=fun_arg+
    { nm, args }
|  arg1=fun_arg nm=op_symbol arg2=fun_arg
    { let nm = Preid.create ~loc:(mk_loc $loc(nm)) nm in
        nm, [arg1; arg2] }

val_spec_header:
| ret=ret_name EQUAL app = val_spec_app
  { let nm, args = app in
    { sp_hd_nm = nm; sp_hd_ret = ret; sp_hd_args = args } }
| app = val_spec_app
  { let nm, args = app in
    { sp_hd_nm = nm; sp_hd_ret = [ Lwild ]; sp_hd_args = args } }
;

val_spec_post:
| ENSURES t=term bd=val_spec_post_empty
  { { bd with sp_post = t :: bd.sp_post} }
| PRODUCES pr=val_spec_own bd=val_spec_post_empty
  { { bd with sp_produces = pr @ bd.sp_produces } }


val_spec_post_empty:
| (* epsilon *) { empty_post_vspec }
| bd=val_spec_post { bd }

fun_arg:
| LEFTPAR RIGHTPAR
  { Lunit (mk_loc $loc) }
| id = lident
  { Lvar id }
| LEFTSQ id=lident COLON ty=typ RIGHTSQ
  { Lghost (id, ty) }
;

ret_value:
| id = lident
  { Lvar id }
| LEFTSQ id=lident COLON ty=typ RIGHTSQ
  { Lghost (id, ty) }
;

ret_pat:
| v = lident
  { [ Lvar v ] }
| l = ret_pat_
  { l }

ret_pat_:
| (* *)
  { [] }
| LEFTPAR RIGHTPAR
  { [ Lunit (mk_loc $loc) ] }
| LEFTPAR l=comma_list(ret_value) RIGHTPAR
  { l }
| UNDERSCORE { [ Lwild ] };

ret_name:
| l = separated_nonempty_list(COMMA, ret_value) { l }
| l = ret_pat_ { l }

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
    { mk_op_apply (mk_pid ~fixity:Mixfix "::" $loc) [l; r] }
| l = term ; o = BACKQUOTE_LIDENT ; r = term
    { mk_op_apply (mk_pid o $loc) [l; r] }
| term_arg located(term_arg)+
    { let join f (a, e) = mk_term (Tapply (f, a)) ($startpos, e) in
      (List.fold_left join $1 $2).term_desc }
| IF g = term THEN t1 = term ELSE t2 = term
    { Tif (g, t1, t2) }
| LET ids = pat EQUAL t1 = term IN t2 = term
   { Tlet(ids, t1, t2) }
| LET id = attrs(lident_op_id) EQUAL t1 = term IN t2 = term
   { Tlet (mk_pat (Pid id) $loc(id), t1, t2) }
| q = quant l = comma_list1(quant_vars) DOT t = term
    { Tquant (q, List.concat l, t) }
| FUN args = pat_arg+ ty = preceded(COLON,fun_typ)? ARROW t = term
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

pat_arg:
| p = pat_arg_ { mk_pat p $loc }

pat_arg_:
| UNDERSCORE { Pwild }
| v=lident { Pid v }
| LEFTPAR p=pat_ RIGHTPAR { p }

pat:
| p=pat_ { mk_pat p $loc }

pat_:
| p=pat_arg_ { p }
| l=comma_list2(pat_arg) { Ptuple l }
| p=pat_arg t=cast { Pcast (p, t) }

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
    { Tvar (Qid (mk_pid ~fixity:Mixfix ( "{}") $loc)) }
| LEFTBRCCOLON t=term COLONRIGHTBRC
    { let id = mk_pid ~fixity:Mixfix ("{:_:}") $loc in
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
| v = FLOAT { Pconst_float v }
| s = STRING { Pconst_string (s, mk_loc $loc) }
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
| o = OPPREF { mk_pid ~fixity:Prefix o $loc }
;

prefix_op:
| o = op_symbol { mk_pid ~fixity:Prefix o $loc }
;

%inline infix_op_1:
| o = OP1   { mk_pid ~fixity:Infix o $loc }
| EQUAL     { mk_pid ~fixity:Infix "=" $loc }
| LTGT      { mk_pid ~fixity:Infix "<>" $loc }
%inline infix_op_234:
| o = OP2   { mk_pid ~fixity:Infix o $loc }
| o = OP3   { mk_pid ~fixity:Infix o $loc }
| STAR      { mk_pid ~fixity:Infix "*" $loc }
| o = OP4   { mk_pid ~fixity:Infix o $loc }
| ARROW     { mk_pid ~fixity:Infix "->"  $loc }
| LRARROW   { mk_pid ~fixity:Infix "<->" $loc }
| DISJ      { mk_pid ~fixity:Infix "\\/" $loc }
| BARBAR    { mk_pid ~fixity:Infix "||"  $loc }
| CONJ      { mk_pid ~fixity:Infix "/\\" $loc }
| AMPAMP    { mk_pid ~fixity:Infix "&&"  $loc }
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
| LEFTPAR DISJ RIGHTPAR            { mk_pid ~fixity:Infix "\\/" $loc }
| LEFTPAR CONJ RIGHTPAR            { mk_pid  ~fixity:Infix "/\\" $loc }
;

lident_op:
| op = op_symbol                              { op }
| op = op_symbol UNDERSCORE                   { op }
| EQUAL                                       { "=" }
| LTGT                                        { "<>" }
| op = OPPREF UNDERSCORE?                     { op }
| DOT LEFTPAR RIGHTPAR                        { ".()" }
| LEFTSQ UNDERSCORE RIGHTSQ                   { "[_]" }
| LEFTSQ ARROW RIGHTSQ                        { "[->]" }
| LEFTSQ UNDERSCORE DOTDOT UNDERSCORE RIGHTSQ { "[_.._]" }
| LEFTSQ            DOTDOT UNDERSCORE RIGHTSQ { "[.._]" }
| LEFTSQ UNDERSCORE DOTDOT            RIGHTSQ { "[_..]" }
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
