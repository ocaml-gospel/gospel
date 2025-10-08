(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Parse_uast
open Utils.Fmt

(* Identifiers *)

let rec qualid fmt = function
  | Qid id -> Preid.pp fmt id
  | Qdot (q, pid) -> pp fmt "@[%a.%a@]" qualid q Preid.pp pid

(* Types *)

let print_tv fmt tv = pp fmt "'%s" tv.Preid.pid_str

let tparams p fmt = function
  | [] -> ()
  | [ x ] -> pp fmt "%a " p x
  | l -> (parens (list ~sep:comma p) ++ sp) fmt l

let rec print_arrow_ty fmt = list ~sep:arrow print_ty fmt

and print_tuple_par fmt ty =
  match ty with
  | PTtuple _ | PTarrow _ -> pp fmt "%a" (parens print_ty) ty
  | _ -> print_ty fmt ty

and print_ty fmt = function
  | PTtyvar v -> pp fmt "%a" print_tv v
  | PTarrow ((PTarrow _ as ty1), ty2) ->
      pp fmt "@[%a@ ->@ %a@]" (parens print_ty) ty1 print_ty ty2
  | PTarrow (ty1, ty2) -> print_arrow_ty fmt [ ty1; ty2 ]
  | PTtyapp (q, l) -> pp fmt "@[%a%a@]" (tparams print_ty) l qualid q
  | PTtuple l -> pp fmt "%a" (list ~sep:star print_tuple_par) l

(* Specification arguments *)

let labelled_arg fmt = function
  | Lwild -> pp fmt "_"
  | Lunit _ -> pp fmt "()"
  | Lvar pid -> pp fmt "@[%a@]" Preid.pp pid
  | Lghost (pid, ty) -> pp fmt "@[[%a :@ %a]@]" Preid.pp pid print_ty ty

let labelled_args fmt = function
  | [ arg ] -> labelled_arg fmt arg
  | l -> list ~sep:comma ~first:lparens ~last:rparens labelled_arg fmt l

(* Identifiers with their types. *)

let param =
  let param fmt (id, pty) = pp fmt "@[%a :@ %a@]" Preid.pp id print_ty pty in
  parens param

let binder fmt (id, pty) =
  match pty with None -> Preid.pp fmt id | Some pty -> param fmt (id, pty)

let rec pat fmt p =
  match p.pat_desc with
  | Pwild -> pp fmt "_"
  | Pid id -> Preid.pp fmt id
  | Pcast (p, ty) -> pp fmt "%a :@ %a" pat p print_ty ty
  | Ptuple l -> list ~sep:comma (parens pat) fmt l

(* Terms *)

let constant fmt = function
  | Pconst_integer (n, None) | Pconst_float n -> pp fmt "%s" n
  | Pconst_integer (n, Some c) -> pp fmt "%s%c" n c
  | Pconst_string (s, _) -> pp fmt "%S" s
  | Pconst_char c -> pp fmt "%C" c

let is_symbol ~prefix = function
  | Tvar (Qid v) when prefix = v.pid_fixity -> true
  | _ -> false

let is_infix = is_symbol ~prefix:Infix
let is_prefix = is_symbol ~prefix:Prefix
let is_mixfix = is_symbol ~prefix:Mixfix

let sans_prefix v =
  match String.split_on_char ' ' v.Preid.pid_str with
  | [ _; id ] -> id
  | _ -> assert false

let symbol s = match s with Tvar (Qid v) -> sans_prefix v | _ -> assert false
let pp_symbol fmt s = pp fmt "%s" (sans_prefix s)

let needs_paren arg =
  match arg.term_desc with
  | Tconst _ | Ttrue | Tfalse | Tvar _ | Tfield _ | Trecord _ | Tscope _
  | Tinfix _ ->
      false
  | _ -> true

let print_ids fmt = function
  | [] -> pp fmt "_"
  | [ x ] -> Preid.pp fmt x
  | l -> parens (list ~sep:comma Preid.pp) fmt l

let print_quantifier fmt = function
  | Tforall -> string fmt "forall"
  | Texists -> string fmt "exists"

let rec print_field fmt (q, t) = pp fmt "@[%a =@ %a@]" qualid q term t

and print_arg fmt arg =
  if needs_paren arg then (parens term) fmt arg else term fmt arg

and print_app fmt f arg1 =
  match f.term_desc with
  | Tapply (v, arg2) when is_infix v.term_desc ->
      pp fmt "%a %s@ @[%a@]" print_arg arg1 (symbol v.term_desc) print_arg arg2
  | _ when is_prefix f.term_desc ->
      pp fmt "%s@ @[%a@]" (symbol f.term_desc) print_arg arg1
  | _ -> pp fmt "%a@ @[%a@]" term f print_arg arg1

and term fmt { term_desc; _ } =
  let print_tdesc fmt t_node =
    match t_node with
    | Tconst c -> pp fmt "%a" constant c
    | Ttrue -> pp fmt "true"
    | Tfalse -> pp fmt "false"
    | TTrue -> pp fmt "True"
    | TFalse -> pp fmt "False"
    | Tvar qid -> pp fmt "%a" qualid qid
    | Tfield (t, qid) -> pp fmt "(%a).%a" print_arg t qualid qid
    | Tapply (t1, t2) -> print_app fmt t1 t2
    | Tinfix (t1, id, t2) ->
        pp fmt "%a %a %a" print_arg t1 pp_symbol id print_arg t2
    | Tif (t1, t2, t3) ->
        pp fmt "if %a@ then@ %a else@ %a" term t1 term t2 term t3
    | Tquant (q, vsl, t) ->
        pp fmt "%a %a.@ @[%a@]" print_quantifier q (list ~sep:sp binder) vsl
          term t
    | Tlambda (pl, t, ty) ->
        pp fmt "fun %a%a -> %a" (list ~sep:sp pat) pl
          (option (fun fmt -> pp fmt " : %a" print_ty))
          ty term t
    | Tattr (_, t) -> term fmt t
    | Tlet (ids, t1, t2) ->
        pp fmt "let %a =@ %a in@ @[%a@]" pat ids term t1 term t2
    | Tcast (t, ty) -> pp fmt "%a :@ %a" term t print_ty ty
    | Ttuple l -> parens (list ~sep:comma term) fmt l
    | Trecord l -> braces (list ~sep:semi print_field) fmt l
    | Tscope (q, t) -> pp fmt "%a.%a" qualid q term t
    | Told t -> pp fmt "old (%a)" term t
  in
  pp fmt "%a" print_tdesc term_desc

(* Value specifications *)

let spec_header ~exn fmt header =
  if exn then
    pp fmt "@[match %a %a with@]" Preid.pp header.sp_hd_nm
      (list ~sep:sp labelled_arg)
      header.sp_hd_args
  else
    pp fmt "@[let %a =@ %a %a@]" labelled_args header.sp_hd_ret Preid.pp
      header.sp_hd_nm labelled_args header.sp_hd_args

let spec_clauses clause_pp fmt (keyword, l) =
  let spec_clause fmt t = pp fmt "%s @[%a@]" keyword clause_pp t in
  list ~sep:newline spec_clause fmt l

let condition = spec_clauses term
let ownership = spec_clauses qualid
let ensures post = ("ensures", post)
let requires pre = ("requires", pre)

let pre_spec fmt spec =
  pp fmt "@[%a%a%a%a%a%a@]" condition (requires spec.sp_pre) ownership
    ("consumes", spec.sp_consumes)
    ownership
    ("modifies", spec.sp_modifies)
    ownership
    ("preserves", spec.sp_preserves)
    (if' spec.sp_diverge string)
    "diverges" (if' spec.sp_pure string) "pure"

let post_spec fmt spec =
  pp fmt "%a%a" ownership
    ("produces", spec.sp_produces)
    condition (ensures spec.sp_post)

let xpost_spec fmt spec =
  pp fmt "@[<hov 2>|exception %a %a ->@\n%a%a@]" qualid spec.sp_exn
    labelled_args spec.sp_xrets ownership
    ("produces", spec.sp_xproduces)
    condition (ensures spec.sp_xpost)

let ret_case hd fmt post =
  pp fmt "@[<hov 2>|%a ->@\n%a@]@\n" labelled_args hd.sp_hd_ret post_spec post

let print_post fmt spec =
  let spec_header ~exn = option (spec_header ~exn) in
  let is_xpost_empty = spec.sp_xpost_spec = [] in
  let post_exists =
    not (spec.sp_post_spec.sp_produces = [] && spec.sp_post_spec.sp_post = [])
  in
  if is_xpost_empty then
    pp fmt "@[%a%a@]" (spec_header ~exn:false) spec.sp_header
      (if' post_exists (fun fmt -> pp fmt " in@\n%a" post_spec))
      spec.sp_post_spec
  else
    pp fmt "%a@\n%a%a" (spec_header ~exn:true) spec.sp_header
      (if' post_exists (ret_case (Option.get spec.sp_header)))
      spec.sp_post_spec
      (list ~sep:newline xpost_spec)
      spec.sp_xpost_spec

let val_spec fmt spec =
  pp fmt "@[%a%a@]" pre_spec spec.sp_pre_spec print_post spec

(* Type specifications *)

let model fmt = function
  | No_model -> ()
  | Implicit ty -> pp fmt "model :@ %a@\n" print_ty ty
  | Fields l ->
      let field fmt (id, ty) =
        pp fmt "model %a :@ %a@\n" Preid.pp id print_ty ty
      in
      (list field fmt) l

let invariants fmt (id, l) =
  let invariant fmt t = pp fmt "@[invariant %a@]" term t in
  pp fmt "with %a %a" Preid.pp id (list ~sep:newline invariant) l

let type_spec fmt tspec =
  pp fmt "@[%a%a%a@]"
    (if' tspec.ty_mutable (fun fmt () -> pp fmt "mutable@\n"))
    () model tspec.ty_model (option invariants) tspec.ty_invariant

(* Function specifications *)

let fun_spec fmt fspec =
  pp fmt "@[%a%a%a@]" condition (requires fspec.fun_req) condition
    (ensures fspec.fun_ens) condition
    ("variant", fspec.fun_variant)

let function_ fmt f =
  let keyword = if Option.is_some f.fun_type then "function" else "predicate" in
  pp fmt "@[%s %a %a%a%a%a@]" keyword Preid.pp f.fun_name (list ~sep:sp param)
    f.fun_params
    (option (fun fmt -> pp fmt " :@ %a" print_ty))
    f.fun_type
    (option (fun fmt -> pp fmt " =@ @[%a@]" term))
    f.fun_def fun_spec f.fun_spec

(* Axioms *)

let axiom fmt ax =
  pp fmt "@[<hov 2>axiom %a :@ @[%a@]@]" Preid.pp ax.ax_name term ax.ax_term

(* OCaml values *)

let gospel f fmt x = pp fmt "@[(*@@ %a@ *)@]" f x

let s_val_description fmt v =
  pp fmt "@[@[val %a :@ %a@]@\n%a@]" Preid.pp v.vname print_ty v.vtype
    (option (gospel val_spec))
    v.vspec

(* Type declarations *)

let mutable_flag fmt = function
  | Mutable -> pp fmt "mutable@ "
  | Immutable -> ()

let label_declaration fmt lbl =
  pp fmt "@[%a%a :@ %a@]" mutable_flag lbl.pld_mutable Preid.pp lbl.pld_name
    print_ty lbl.pld_type

let type_kind fmt = function
  | PTtype_abstract -> ()
  | PTtype_record l ->
      pp fmt "@[{@ %a@ }@]" (list ~sep:semi label_declaration) l

let private_flag fmt = function Private -> pp fmt "private@ " | Public -> ()

let s_type_declaration ~ghost fmt decl =
  let eq fmt () =
    match decl.tkind with
    | PTtype_abstract when Option.is_none decl.tmanifest -> ()
    | _ -> pp fmt " =@ "
  in
  (* If this is a ghost type, we do not need to wrap the type
     specification in a Gospel comment *)
  let gospel = if ghost then fun p fmt x -> p fmt x else gospel in
  pp fmt "%a%a%a%a%a@]%a" (tparams print_tv) decl.tparams Preid.pp decl.tname eq
    () type_kind decl.tkind (option print_ty) decl.tmanifest
    (option (newline ++ gospel type_spec))
    decl.tspec

let tdecl_list ~ghost =
  let first fmt () = pp fmt "@[type " in
  let sep fmt () = pp fmt "@[and " in
  list ~first ~sep (s_type_declaration ~ghost)

(* Module opens *)

let mod_open fmt q = pp fmt "@[open %a@]" qualid q

(* Gospel Signatures *)

let gospel_signature fmt = function
  | Sig_function f -> function_ fmt f
  | Sig_axiom ax -> axiom fmt ax
  | Sig_ghost_type l -> tdecl_list ~ghost:true fmt l
  | Sig_ghost_open q -> mod_open fmt q

(* Exceptional declarations *)

let exception_decl fmt e =
  let args fmt = function
    | [] -> ()
    | l -> pp fmt " of@ %a" (list ~sep:star print_ty) l
  in
  pp fmt "@[exception %a%a@]" Preid.pp e.exn_id args e.exn_args

(* Signatures *)

let rec signature_item fmt x =
  match x.sdesc with
  | Sig_val v -> s_val_description fmt v
  | Sig_type t -> tdecl_list ~ghost:false fmt t
  | Sig_module m -> module_decl fmt m
  | Sig_exception e -> exception_decl fmt e
  | Sig_open q -> mod_open fmt q
  | Sig_gospel (g, _) -> (gospel gospel_signature) fmt g
  | Sig_attribute _ -> string fmt "[@@@ attribute]"

and module_decl fmt m =
  let (Mod_signature l) = m.mdtype.mdesc in
  pp fmt "module %a : sig@\n@[%a@]" Preid.pp m.mdname signature l

and signature fmt x =
  Format.pp_open_hovbox fmt 2;
  newline fmt ();
  list ~sep:(newline ++ newline) signature_item fmt x
