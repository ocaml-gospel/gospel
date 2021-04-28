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
  let set_op l = Qpreid (mk_pid (mixfix "[<-]") l)
  let sub_op l = Qpreid (mk_pid (mixfix "[_.._]") l)
  let above_op l = Qpreid (mk_pid (mixfix "[_..]") l)
  let below_op l = Qpreid (mk_pid (mixfix "[.._]") l)

  let id_anonymous loc = Preid.create "_" ~attrs:[] ~loc

  let array_get l =
    Qdot (Qpreid (mk_pid "Array" l), mk_pid (mixfix "[_]") l)

  let empty_vspec = {
    sp_hd_ret = [];
    sp_hd_nm = mk_pid "" (Lexing.dummy_pos, Lexing.dummy_pos);
    sp_hd_args = [];
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
  }

  let empty_fspec = {
    fun_req = [];
    fun_ens = [];
    fun_variant = [];
    fun_coer = false;
  }

  let empty_tspec = {
    ty_ephemeral = false;
    ty_field = [];
    ty_invariant = [];
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

%token <string> INTEGER
%token <string> FLOAT
%token <char> CHAR
%token <string> STRING

(* Spec Tokens *)

%token REQUIRES ENSURES CONSUMES VARIANT

(* keywords *)

%token AXIOM
%token EPHEMERAL ELSE EXISTS FALSE FORALL FUNCTION FUN
%token REC
%token INVARIANT
%token COERCION
%token IF IN
%token OLD NOT RAISES
%token THEN TRUE MODIFIES EQUIVALENT CHECKS DIVERGES PURE

%token AS
%token LET MATCH PREDICATE
%token WITH

(* symbols *)

%token AND AMPAMP ARROW BAR BARBAR COLON COLONCOLON COMMA DOT DOTDOT
%token EOF EQUAL
%token MUTABLE MODEL
%token LARROW LRARROW LEFTBRC LEFTBRCCOLON LEFTPAR LEFTBRCRIGHTBRC
%token LEFTSQ LTGT OR QUESTION RIGHTBRC COLONRIGHTBRC RIGHTPAR RIGHTSQ SEMICOLON
%token LEFTSQRIGHTSQ
%token STAR TILDE UNDERSCORE

(* priorities *)

%nonassoc IN
%nonassoc DOT ELSE
%nonassoc prec_named
%right COLON

%right ARROW LRARROW
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
| hd=val_spec_header bd=val_spec_body EOF
  { let sp_hd_ret, sp_hd_nm, sp_hd_args = hd in
    { bd with sp_hd_ret; sp_hd_nm; sp_hd_args } }
(* TODO: pure or diverges only *)
;

axiom:
| AXIOM id=lident COLON t=term EOF
  { {ax_name = id; ax_term = t; ax_loc = mk_loc $loc} }
;

func:
| FUNCTION fun_rec=boption(REC) fun_name=func_name fun_params=loption(params)
    COLON ty=typ fun_def=preceded(EQUAL, term)? EOF
  { { fun_name; fun_rec; fun_type = Some ty; fun_params; fun_def; fun_spec = None;
      fun_loc = mk_loc $loc } }
| PREDICATE fun_rec=boption(REC) fun_name=func_name fun_params=params
    fun_def=preceded(EQUAL, term)? EOF
  { { fun_name; fun_rec; fun_type = None; fun_params; fun_def; fun_spec = None;
      fun_loc = mk_loc $loc } }
;

func_name:
| lident_rich {$1}
| LEFTPAR LEFTBRCRIGHTBRC RIGHTPAR
  { mk_pid (mixfix "{}") $loc }
| LEFTPAR LEFTBRCCOLON UNDERSCORE COLONRIGHTBRC RIGHTPAR
  { mk_pid (mixfix "{:_:}") $loc }

func_spec:
| EOF { empty_fspec }
| nonempty_func_spec EOF { $1 }

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
| EOF { empty_tspec }
| nonempty_type_spec EOF { $1 }

nonempty_type_spec:
| EPHEMERAL ts=type_spec
  { { ts with ty_ephemeral = true } }
| field=type_spec_model ts=type_spec
  { { ts with
      ty_field = field :: ts.ty_field;
      ty_ephemeral = ts.ty_ephemeral || field.f_mutable } }
| INVARIANT inv=term ts=type_spec
  { { ts with ty_invariant = inv :: ts.ty_invariant } }
;

type_spec_model:
| f_mutable=boption(MUTABLE) MODEL f_preid=lident_rich COLON f_pty=typ
  { { f_preid; f_mutable; f_pty;
      f_loc = mk_loc $loc } }
;

val_spec_header:
| ret=ret_name nm=lident_rich args=fun_arg*
  { ret, nm, args }
| nm=lident_rich args=fun_arg*
  { [], nm, args }
;

val_spec_body:
| (* Empty spec *) { empty_vspec }
| PURE bd=val_spec_body
  { {bd with sp_pure = true} }
| DIVERGES bd=val_spec_body
  { {bd with sp_diverge = true} }
| MODIFIES wr=separated_list(COMMA, term) bd=val_spec_body
  { { bd with sp_writes = wr @ bd.sp_writes } }
| CONSUMES cs=separated_list(COMMA, term) bd=val_spec_body
  { { bd with sp_consumes = cs @ bd.sp_consumes } }
| REQUIRES t=term bd=val_spec_body
  { { bd with sp_pre = t :: bd.sp_pre } }
| CHECKS t=term bd=val_spec_body
  { { bd with sp_checks = t :: bd.sp_pre } }
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
| lident
  { Lnone $1 }
| TILDE lident
  { Lnamed $2 }
| QUESTION lident
  { Loptional $2 }
| LEFTSQ id=lident COLON ty=typ RIGHTSQ
  { Lghost (id, ty) }
;

ret_value:
| lident
  { Lnone $1 }
| LEFTSQ id=lident COLON ty=typ RIGHTSQ
  { Lghost (id, ty) }
;

ret_name:
| LEFTPAR comma_list(ret_value) RIGHTPAR EQUAL
  { $2 }
| comma_list(ret_value) EQUAL { $1 } ;

raises:
| q=uqualid ARROW t=term
  { q, Some (mk_pat (Ptuple []) $loc(q), t) }
| q=uqualid p=pat_arg ARROW t=term
  { q, Some (p, t) }
| q=uqualid
  { q, None}
;

params:
| param  { $1 }
| param params { $1 @ $2 }
;

param:
| LEFTPAR params=lident+ COLON t=ty RIGHTPAR
  { List.map (fun x -> mk_loc $loc, x, t) params }
;

cast:
| COLON ty_arg  { $2 }
;

term: t = mk_term(term_) { t }
;

term_:
| term_arg_
    { $1 }
| NOT term
    { Tnot $2 }
| OLD term
    { Told $2 }
| prefix_op term %prec prec_prefix_op
    { Tidapp (Qpreid $1, [$2]) }
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
| term_arg located(term_arg)+
    { let join f (a, _ ,e) = mk_term (Tapply (f, a)) ($startpos, e) in
      (List.fold_left join $1 $2).term_desc }
| IF term THEN term ELSE term
    { Tif ($2, $4, $6) }
| LET pattern EQUAL term IN term
    { let cast ty = { $4 with term_desc = Tcast ($4, ty) } in
      let pat, def = match $2.pat_desc with
        | Ptuple [] -> { $2 with pat_desc = Pwild }, cast (PTtuple [])
        | Pcast ({pat_desc = (Pvar _|Pwild)} as p, ty) -> p, cast ty
        | _ -> $2, $4 in
      match pat.pat_desc with
      | Pvar id -> Tlet (id, def, $6)
      | Pwild -> Tlet (id_anonymous pat.pat_loc, def, $6)
      | _ -> Tcase (def, [pat, $6]) }
| LET attrs(lident_op_id) EQUAL term IN term
    { Tlet ($2, $4, $6) }
| MATCH term WITH match_cases(term)
    { Tcase ($2, $4) }
| MATCH comma_list2(term) WITH match_cases(term)
    { Tcase (mk_term (Ttuple $2) $loc($2), $4) }
| quant comma_list1(quant_vars) triggers DOT term
    { Tquant ($1, List.concat $2, $3, $5) }
| FUN args = quant_vars ARROW t = term
    { Tquant (Tlambda, args, [], t) }
| attr term %prec prec_named
    { Tattr ($1, $2) }
| term cast
    { Tcast ($1, $2) }
;

field_list1(X):
| fl = semicolon_list1(term_rec_field(X)) { fl }
;

term_rec_field(X):
| separated_pair(lqualid, EQUAL, X) { $1 }
| lqualid { let t = {term_desc = Tpreid $1;
                     term_loc = loc_of_qualid $1} in
            ($1,t)
          }
;

match_cases(X):
| cl = bar_list1(separated_pair(pattern, ARROW, X)) { cl }
;

quant_vars:
| binder_var+ cast? { List.map (fun id -> id, $2) $1 }
;

triggers:
| (* epsilon *)                                                 { [] }
| LEFTSQ separated_nonempty_list(BAR,comma_list1(term)) RIGHTSQ { $2 }
;

attrs(X): X attr* { List.fold_left (fun acc s -> Preid.add_attr acc s) $1 $2 }
;

attr:
| ATTRIBUTE    { $1 }
;

term_arg: mk_term(term_arg_) { $1 };
term_dot: mk_term(term_dot_) { $1 }
;

term_arg_:
| qualid                    { Tpreid $1 }
| constant                  { Tconst $1 }
| TRUE                      { Ttrue }
| FALSE                     { Tfalse }
| o = oppref ; a = term_arg { Tidapp (Qpreid o, [a]) }
| term_sub_                 { $1 }
;

term_dot_:
| lqualid                   { Tpreid $1 }
| o = oppref ; a = term_dot { Tidapp (Qpreid o, [a]) }
| term_sub_                 { $1 }
;

term_block_:
| LEFTPAR t=term RIGHTPAR                           { t.term_desc }
| LEFTPAR RIGHTPAR                                  { Ttuple [] }
| LEFTSQRIGHTSQ
    { Tpreid (Qpreid (mk_pid "[]"  $loc)) }
| LEFTBRC field_list1(term) RIGHTBRC                { Trecord $2 }
| LEFTBRC term_arg WITH field_list1(term) RIGHTBRC  { Tupdate ($2,$4) }
| LEFTBRCRIGHTBRC
    { Tpreid (Qpreid (mk_pid (mixfix "{}") $loc)) }
| LEFTBRCCOLON t=term COLONRIGHTBRC
    { let id = Qpreid (mk_pid (mixfix "{:_:}") $loc) in
      Tidapp (id, [t]) }
;

term_sub_:
| term_block_                                       { $1 }
| uqualid DOT mk_term(term_block_)                  { Tscope ($1, $3) }
| term_dot DOT lqualid_rich                         { Tidapp ($3,[$1]) }
| term_arg LEFTSQ term RIGHTSQ
    { Tidapp (get_op $loc($2), [$1;$3]) }
| term_arg LEFTSQ term LARROW term RIGHTSQ
    { Tidapp (set_op $loc($2), [$1;$3;$5]) }
| term_arg LEFTSQ term DOTDOT term RIGHTSQ
    { Tidapp (sub_op $loc($2), [$1;$3;$5]) }
| term_arg LEFTSQ term DOTDOT RIGHTSQ
    { Tidapp (above_op $loc($2), [$1;$3]) }
| term_arg LEFTSQ DOTDOT term RIGHTSQ
    { Tidapp (below_op $loc($2), [$1;$4]) }
| LEFTPAR comma_list2(term) RIGHTPAR                { Ttuple $2 }
| term_dot DOT LEFTPAR term RIGHTPAR
    { Tidapp (array_get $loc($2), [$1; $4]) }
| t1=term_dot DOT LEFTPAR t2=term LARROW t3=term  RIGHTPAR
    { Tidapp (set_op $loc($2), [t1;t2;t3]) }
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
| INTEGER { Parsetree.Pconst_integer ($1, None) }
| FLOAT { Parsetree.Pconst_float ($1, None) }
| STRING { Pconst_string ($1, mk_loc $loc, None) }
| CHAR { Pconst_char $1 }
;

binder_var:
| attrs(lident)  { $1 }
;

mk_expr(X): d = X { mk_expr d $loc }
;

typ:
| ty_arg
    { $1 }
| id=lident COLON aty=typ ARROW rty=typ
    { PTarrow (Lnamed id, aty, rty) }
| QUESTION id=lident COLON aty=typ ARROW rty=typ
    { PTarrow (Loptional id, aty, rty) }
| typ ARROW typ
    { let l = mk_loc $loc in
      PTarrow (Lnone (id_anonymous l), $1, $3) }
| ty_arg STAR ty_tuple
    { PTtuple ($1 :: $3) }
;

ty_tuple:
| ty_arg
    { [$1] }
| ty_arg STAR ty_tuple
    { $1 :: $3 }
;

ty_arg:
| lqualid
    { PTtyapp ($1, []) }
| quote_lident
    { PTtyvar $1 }
| LEFTPAR typ RIGHTPAR
    { $2 }
| ty_arg lqualid
    { PTtyapp ($2, [$1]) }
| LEFTPAR typ COMMA separated_nonempty_list(COMMA, typ)  RIGHTPAR id=lqualid
    { PTtyapp (id, $2::$4) }
;

%inline ty:
| typ {$1}
;

mk_term(X): d = X { mk_term d $loc }
;

(* Patterns *)

mk_pat(X): X { mk_pat $1 $loc }
;

pattern: mk_pat(pattern_) { $1 };
pat_arg: mk_pat(pat_arg_) { $1 }
;

pattern_:
| pat_conj_                             { $1 }
| mk_pat(pat_conj_) BAR pattern         { Por ($1,$3) }
;

pat_conj_:
| pat_uni_                              { $1 }
| comma_list2(mk_pat(pat_uni_))         { Ptuple $1 }
;

pat_uni_:
| pat_arg_                              { $1 }
| pat_arg COLONCOLON pat_arg
    { Papp (Qpreid (mk_pid (infix "::") $loc),[$1;$3]) }
| uqualid pat_arg+                      { Papp ($1,$2) }
| mk_pat(pat_uni_) AS attrs(lident)
                                        { Pas ($1,$3) }
| mk_pat(pat_uni_) cast                 { Pcast ($1, $2) }
;

pat_arg_:
| pat_arg_shared_                       { $1 }
| attrs(lident)                         { Pvar $1 }
;

pat_arg_shared_:
| UNDERSCORE                            { Pwild }
| uqualid                               { Papp ($1,[]) }
| LEFTPAR RIGHTPAR                      { Ptuple [] }
| LEFTSQRIGHTSQ
  { Papp (Qpreid (mk_pid "[]"  $loc), []) }
| LEFTPAR pattern_ RIGHTPAR             { $2 }
| LEFTBRC field_pattern(pattern) RIGHTBRC { Prec $2 }
;

field_pattern(X):
| fl = semicolon_list1(pattern_rec_field(X)) { fl }
;

pattern_rec_field(X):
| separated_pair(lqualid, EQUAL, X) { $1 }
| lqualid { let p = {pat_desc = Pvar (qualid_preid $1);
                     pat_loc = loc_of_qualid $1} in
            ($1,p)
          }
;

(* Symbolic operation names *)

op_symbol:
| OP1  { $1 }
| OP2  { $1 }
| OP3  { $1 }
| OP4  { $1 }
| STAR { "*" }
;

%inline oppref:
| o = OPPREF { mk_pid (prefix o) $loc }
;

prefix_op:
| op_symbol { mk_pid (prefix $1) $loc }
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
| LIDENT        { mk_pid $1 $loc }
;

uident:
| UIDENT        { mk_pid $1 $loc }
;

quote_lident:
| QUOTE_LIDENT  { mk_pid $1 $loc }
;

ident_rich:
| uident        { $1 }
| lident_rich   { $1 }
;

lident_rich:
| lident        { $1 }
| lident_op_id  { $1 }
;

lident_op_id:
| LEFTPAR lident_op RIGHTPAR  { mk_pid $2 $loc }
;

lident_op:
| op_symbol                                   { infix $1 }
| op_symbol UNDERSCORE                        { prefix $1 }
| EQUAL                                       { infix "=" }
| OPPREF UNDERSCORE?                          { prefix $1 }
| DOT LEFTPAR RIGHTPAR                        { mixfix ".()" }
| DOT LEFTPAR LARROW RIGHTPAR                 { mixfix ".(<-)" }
| LEFTSQ UNDERSCORE RIGHTSQ                   { mixfix "[_]" }
| LEFTSQ LARROW RIGHTSQ                       { mixfix "[<-]" }
| LEFTSQ UNDERSCORE DOTDOT UNDERSCORE RIGHTSQ { mixfix "[_.._]" }
| LEFTSQ            DOTDOT UNDERSCORE RIGHTSQ { mixfix "[.._]" }
| LEFTSQ UNDERSCORE DOTDOT            RIGHTSQ { mixfix "[_..]" }
;

(* Qualified idents *)

qualid:
| ident_rich              { Qpreid $1 }
| uqualid DOT ident_rich  { Qdot ($1, $3) }
;

lqualid_rich:
| lident_rich             { Qpreid $1 }
| uqualid DOT lident_rich { Qdot ($1, $3) }
;

lqualid:
| lident              { Qpreid $1 }
| uqualid DOT lident  { Qdot ($1, $3) }
;

uqualid:
| uident              { Qpreid $1 }
| uqualid DOT uident  { Qdot ($1, $3) }
;

(* Miscellaneous *)

comma_list(X):
| separated_nonempty_list(COMMA, X) { $1 }
;

bar_list1(X):
| ioption(BAR) ; xl = separated_nonempty_list(BAR, X) { xl }
;

with_list1(X):
| separated_nonempty_list(WITH, X)  { $1 }
;

comma_list2(X):
| X COMMA comma_list1(X) { $1 :: $3 }
;

comma_list1(X):
| separated_nonempty_list(COMMA, X) { $1 }
;

semicolon_list1(X):
| x = X ; ioption(SEMICOLON)                  { [x] }
| x = X ; SEMICOLON ; xl = semicolon_list1(X) { x :: xl }
;

star_list2(X):
| X STAR separated_nonempty_list(STAR, X) { $1 :: $3 }
;

located(X): X { $1, $startpos, $endpos }
;
