(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Id_uast
open Tast
open Utils.Fmt

type ty_print_level = Minimal | Bindings | All

(** This flag is used to keep track of how identifiers and their types should be
    printed. *)
let ty_flag = ref Minimal

(* Identifiers *)
let print_ty = Types.print_ty

let cast fmt id =
  let cast fmt () = pp fmt "%a : %a" Ident.pp id.ts_id print_ty id.ts_ty in
  parens cast fmt ()

let ts ~top_level fmt v =
  let type_is_printed =
    match !ty_flag with All | Bindings -> true | Minimal -> top_level
  in
  if type_is_printed then pp fmt "@[%a@]" cast v

let rec pat fmt p =
  match p.pat_desc with
  | Pwild -> pp fmt "_"
  | Pid id -> ts ~top_level:false fmt id
  | Pcast (p, t) -> pp fmt "(%a :@ %a)" pat p print_ty t
  | Ptuple l -> list ~sep:comma (parens pat) fmt l

let print_pats fmt l = list ~sep:sp pat fmt l

let rec qualid fmt = function
  | Qid id -> Ident.pp fmt id
  | Qdot (q, pid) -> pp fmt "@[%a.%a@]" qualid q Ident.pp pid

let print_tids fmt = function
  | [] -> pp fmt "_"
  | [ x ] -> ts ~top_level:false fmt x
  | l -> parens (list ~sep:comma (ts ~top_level:false)) fmt l

let is_symbol ~prefix = function
  | Tvar (Qid v) when prefix = v.id_fixity -> true
  | _ -> false

let symbol s = match s with Tvar (Qid v) -> v.id_str | _ -> assert false
let is_infix = is_symbol ~prefix:Preid.Infix
let is_prefix = is_symbol ~prefix:Preid.Prefix
let is_mixfix = is_symbol ~prefix:Preid.Mixfix

let needs_paren arg =
  match arg.t_node with
  | Tconst _ | Ttrue | Tfalse | Ttyapply _ | Tvar _ | Tfield _ | Trecord _
  | Tscope _ ->
      false
  | _ -> true

let rec print_field fmt (q, t) = pp fmt "@[%a =@ %a@]" qualid q term t

and print_arg fmt (arg : Tast.term) =
  if needs_paren arg then (parens term) fmt arg else term fmt arg

and print_app fmt f (arg1 : Tast.term) =
  match f.t_node with
  | Tapply (v, arg2) when is_infix v.t_node ->
      pp fmt "%a %s@ @[%a@]" print_arg arg1 (symbol v.t_node) print_arg arg2
  | _ when is_prefix f.t_node ->
      pp fmt "%s@ @[%a@]" (symbol f.t_node) print_arg arg1
  | _ -> pp fmt "%a@ @[%a@]" term f print_arg arg1

and term fmt t =
  let ts = ts ~top_level:false in
  let term_desc fmt = function
    | Ttrue -> pp fmt "true"
    | Tfalse -> pp fmt "false"
    | TTrue -> pp fmt "True"
    | TFalse -> pp fmt "False"
    | Tvar q | Ttyapply (q, _) -> qualid fmt q
    | Tlet (pats, t1, t2) ->
        pp fmt "let %a =@ %a in@ @[%a@]" pat pats term t1 term t2
    | Tconst c -> Uast_printer.constant fmt c
    | Tapply (t1, t2) -> print_app fmt t1 t2
    | Tif (t1, t2, t3) ->
        pp fmt "if %a@ then@ %a else@ %a" term t1 term t2 term t3
    | Tset (v, t) -> pp fmt "@[{ %a | %a }@]" ts v term t
    | Tquant (q, tids, t) ->
        pp fmt "%a %a.@ @[%a@]" Uast_printer.print_quantifier q
          (list ~sep:sp ts) tids term t
    | Tlambda (pl, t, ty) ->
        pp fmt "fun %a%a -> %a" print_pats pl
          (option (fun fmt -> pp fmt " : %a" print_ty))
          ty term t
    | Tattr (_, t) -> term fmt t
    | Tcast (t, ty) -> pp fmt "%a :@ %a" term t print_ty ty
    | Ttuple l -> parens (list ~sep:comma term) fmt l
    | Trecord l -> braces (list ~sep:semi print_field) fmt l
    | Tscope (q, t) -> pp fmt "%a.%a" qualid q term t
    | Told t -> pp fmt "old (%a)" term t
    | Tfield (t, qid) -> pp fmt "(%a).%a" print_arg t qualid qid
  in
  if !ty_flag = All then
    let cast fmt t =
      let cast fmt () = pp fmt "%a : %a" term_desc t.t_node print_ty t.t_ty in
      parens cast fmt ()
    in
    pp fmt "@[%a@]" cast t
  else term_desc fmt t.t_node

(* Axioms *)

let axiom fmt ax =
  pp fmt "@[<hov 2>axiom %a :@ @[%a@]@]" Ident.pp ax.ax_name term ax.ax_term

let gospel = Uast_printer.gospel
let spec_clauses = Uast_printer.spec_clauses
let condition = spec_clauses term
let ensures post = ("ensures", post)
let requires pre = ("requires", pre)

let fun_spec fmt fspec =
  pp fmt "@[%a%a%a@]" condition (requires fspec.fun_req) condition
    (ensures fspec.fun_ens) condition
    ("variant", fspec.fun_variant)

let function_ fmt f =
  pp fmt "@[function %a %a :@ %a%a%a@]" Ident.pp f.fun_name
    (list ~sep:sp (ts ~top_level:true))
    f.fun_params print_ty f.fun_ret
    (option (fun fmt -> pp fmt " =@ @[%a@]" term))
    f.fun_def fun_spec f.fun_spec

(* Type specifications *)

let model fmt = function
  | No_model -> ()
  | Implicit ty -> pp fmt "model :@ %a@\n" print_ty ty
  | Fields l ->
      let field fmt (id, ty) =
        pp fmt "model %a :@ %a@\n" Ident.pp id print_ty ty
      in
      (list field fmt) l

let invariants fmt (id, l) =
  let invariant fmt t = pp fmt "@[invariant %a@]" term t in
  pp fmt "with %a %a" Ident.pp id (list ~sep:newline invariant) l

let type_spec fmt tspec =
  pp fmt "@[%a%a%a@]"
    (if' tspec.ty_mutable (fun fmt () -> pp fmt "mutable@\n"))
    () model tspec.ty_model (option invariants) tspec.ty_invariant

(* Type declarations *)

let print_tv fmt tv = pp fmt "'%s" tv.Ident.id_str

let tparams p fmt = function
  | [] -> ()
  | [ x ] -> pp fmt "%a " p x
  | l -> (parens (list ~sep:comma p) ++ sp) fmt l

let label_declaration fmt lbl =
  pp fmt "@[%a%a :@ %a@]" Uast_printer.mutable_flag lbl.pld_mutable Ident.pp
    lbl.pld_name print_ty lbl.pld_type

let type_kind fmt = function
  | PTtype_abstract -> ()
  | PTtype_record l ->
      pp fmt "@[{@ %a@ }@]" (list ~sep:semi label_declaration) l

let s_type_declaration ~ghost fmt decl =
  let eq fmt () =
    match decl.tkind with
    | PTtype_abstract when Option.is_none decl.tmanifest -> ()
    | _ -> pp fmt " =@ "
  in
  (* If this is a ghost type, we do not need to wrap the type
     specification in a Gospel comment *)
  let gospel = if ghost then fun p fmt x -> p fmt x else gospel in
  pp fmt "%a%a%a%a%a@]%a" (tparams print_tv) decl.tparams Ident.pp decl.tname eq
    () type_kind decl.tkind (option print_ty) decl.tmanifest
    (option (newline ++ gospel type_spec))
    (if decl.tspec.ty_model = No_model && decl.tspec.ty_invariant = None then
       None
     else Some decl.tspec)

let tdecl_list ~ghost =
  let first fmt () = pp fmt "@[type " in
  let sep fmt () = pp fmt "@[and " in
  list ~first ~sep (s_type_declaration ~ghost)

(* Value specifications *)

let labelled_arg fmt arg =
  match arg with
  | Ghost (id, _) -> Ident.pp fmt id
  | OCaml v -> qualid fmt v.var_name
  | _ -> pp fmt "_"

let labelled_args fmt = function
  | [ arg ] -> labelled_arg fmt arg
  | l -> list ~sep:comma ~first:lparens ~last:rparens labelled_arg fmt l

let spec_header ~exn nm fmt spec =
  if exn then
    pp fmt "@[match %a %a with@]" Ident.pp nm
      (list ~sep:sp labelled_arg)
      spec.sp_args
  else
    pp fmt "@[let %a =@ %a %a@]" labelled_args spec.sp_rets Ident.pp nm
      labelled_args spec.sp_args

let spec_clauses clause_pp fmt (keyword, l) =
  let spec_clause fmt t = pp fmt "%s @[%a@]" keyword clause_pp t in
  list ~sep:newline spec_clause fmt l

let ownership = spec_clauses labelled_arg
let is_owned_var ~cons v = if cons then v.cons else v.prod

let is_owned ~cons v =
  match v with
  | Ghost _ | Wildcard | Unit -> false
  | OCaml v -> is_owned_var ~cons v

let pre_spec fmt spec =
  pp fmt "@[%a%a%a%a@]" condition (requires spec.sp_pre) ownership
    ("consumes", List.filter (is_owned ~cons:true) spec.sp_args)
    (if' spec.sp_diverge string)
    "diverges" (if' spec.sp_pure string) "pure"

let post_spec fmt spec =
  pp fmt "%a%a" ownership
    ( "produces",
      List.filter (is_owned ~cons:false) (spec.sp_args @ spec.sp_rets) )
    condition (ensures spec.sp_post)

let xpost_spec fmt spec =
  pp fmt "@[<hov 2>|exception %a %a ->@\n%a%a@]" qualid spec.sp_exn
    labelled_args spec.sp_xrets ownership
    ( "produces",
      List.filter (is_owned ~cons:false) (spec.sp_xargs @ spec.sp_xrets) )
    condition (ensures spec.sp_xpost)

let ret_case spec fmt post =
  pp fmt "@[<hov 2>|%a ->@\n%a@]@\n" labelled_args spec.sp_rets post_spec post

let print_post nm fmt spec =
  let spec_header ~exn = spec_header ~exn nm in
  let is_xpost_empty = spec.sp_xspec = [] in
  if is_xpost_empty then
    pp fmt "@[%a in@\n%a@]" (spec_header ~exn:false) spec post_spec spec
  else
    pp fmt "%a@\n%a%a" (spec_header ~exn:true) spec (ret_case spec) spec
      (list ~sep:newline xpost_spec)
      spec.sp_xspec

let val_spec nm fmt spec = pp fmt "@[%a%a@]" pre_spec spec (print_post nm) spec

(* Value description *)

let s_val_description fmt v =
  pp fmt "@[@[val %a :@ %a@]@\n%a@]" Ident.pp v.vname print_ty v.vtype
    (option (gospel (val_spec v.vname)))
    v.vspec

(* Exception declarations *)

let exception_decl fmt e =
  let args fmt = function
    | [] -> ()
    | l -> pp fmt " of@ %a" (list ~sep:star print_ty) l
  in
  pp fmt "@[exception %a%a@]" Ident.pp e.exn_id args e.exn_args

(* Module opens *)

let mod_open fmt q = pp fmt "@[open %a@]" qualid q

(* Top level signatures *)

let rec signature_item fmt x =
  match x.sdesc with
  | Sig_value v -> s_val_description fmt v
  | Sig_function f -> function_ fmt f
  | Sig_axiom ax -> axiom fmt ax
  | Sig_type t -> tdecl_list ~ghost:false fmt t
  | Sig_ghost_type t -> tdecl_list ~ghost:true fmt t
  | Sig_module m -> module_decl fmt m
  | Sig_exception e -> exception_decl fmt e
  | Sig_ghost_open q -> mod_open fmt q
  | Sig_attribute _ -> string fmt "[@@@ attribute]"

and module_decl fmt m =
  let (Mod_signature l) = m.mdtype.mdesc in
  pp fmt "module %a : sig@\n@[%a@]" Ident.pp m.mdname signature l

and signature fmt x =
  Format.pp_open_hovbox fmt 2;
  newline fmt ();
  list ~sep:(newline ++ newline) signature_item fmt x

let signature ~verbose fmt x =
  if verbose then ty_flag := Bindings;
  signature fmt x
