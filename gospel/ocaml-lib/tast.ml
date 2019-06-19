open Utils
open Identifier
open Ttypes
open Tterm

(** signatures / top level declarations *)

type attrs = Oparsetree.attributes

type lb_arg =
  | Lnone     of vsymbol
  | Lquestion of vsymbol
  | Lnamed    of vsymbol
  | Lghost    of vsymbol

let vs_of_lb_arg = function
  | Lnone     vs -> vs
  | Lquestion vs -> vs
  | Lnamed    vs -> vs
  | Lghost    vs -> vs

type pre  = term * bool (* whether it is a checks *)
type post = term
type invariant = term list

type val_spec = {
    sp_args    : lb_arg list;
    sp_ret     : lb_arg list; (* can only be Lnone or Lghost *)
    sp_pre     : pre list;
    sp_post    : post list;
    sp_xpost   : (pattern * post) list Mxs.t;
    (* sp_reads   : qualid list;TODO *)
    sp_wr  : term list;
    (* sp_alias   : (term * term) list; TODO *)
    sp_diverge : bool;
    sp_equiv   : string list;
}

exception DuplicatedArg of vsymbol

let val_spec args ret pre post xpost wr dv equiv = {
    sp_args    = args;
    sp_ret     = ret;
    sp_pre     = pre;
    sp_post    = post;
    sp_xpost   = xpost;
    (* sp_reads   : qualid list;TODO *)
    sp_wr  = wr;
    (* sp_alias   : (term * term) list; TODO *)
    sp_diverge = dv;
    sp_equiv   = equiv;
}

(* Checks the following:
   1 - no duplicated args
   2 - pre and post of type prop

   TODO:
   1 - check what to do with writes
   2 - sp_xpost sp_reads sp_alias *)
let mk_val_spec args ret pre post wr dv equiv =
  let add args a =
    let vs = vs_of_lb_arg a in
    check (not(Svs.mem vs args)) (DuplicatedArg vs);
    Svs.add vs args in
  ignore(List.fold_left add Svs.empty args);
  let ty_check ty t = t_ty_check t ty in
  List.iter (fun (t,_) -> ty_check None t) pre;
  List.iter (ty_check None) post;
  val_spec args ret pre post wr dv equiv

type val_description = {
    vd_name  : ident;
    vd_type  : Oparsetree.core_type;
    vd_prim  : string list; (* primitive declaration *)
    vd_attrs : attrs;
    vd_spec  : val_spec option;
    vd_loc   : Location.t;
}

let val_description id cty prim attrs spec loc = {
    vd_name  = id;
    vd_type  = cty;
    vd_prim  = prim;
    vd_attrs = attrs;
    vd_spec  = spec;
    vd_loc   = loc;
}

(* Checks the following:
   1 - TODO

*)
let mk_val_description id cty prim attrs spec loc =
  val_description id cty prim attrs spec loc

type type_spec = {
  ty_ephemeral : bool;
  ty_field     : lsymbol list;
  ty_invariant : invariant;
}

let type_spec ty_ephemeral ty_field ty_invariant =
  {ty_ephemeral; ty_field; ty_invariant }

type mutable_flag = Immutable | Mutable

(* used for both record declarations and variant declarations *)
type 'a label_declaration = {
    ld_field : 'a;
    ld_mut   : mutable_flag;
    ld_loc   : Location.t;
    ld_attrs : attrs; (* l : T [@id1] [@id2] *)
  }

let label_declaration ld_field ld_mut ld_loc ld_attrs =
  {ld_field;ld_mut;ld_loc;ld_attrs}

let get_pjl_of_ld ldl =
  let get acc ld = ld.ld_field in
  List.fold_left get [] ldl

type rec_declaration = {
    rd_cs  : lsymbol;
    rd_ldl : lsymbol label_declaration list
}

type constructor_decl = {
    cd_cs    : lsymbol;                (* constructor *)
    (* cd_ld is empty if defined through a tuple *)
    cd_ld    : (ident * ty) label_declaration list;
    cd_loc   : Location.t;
    cd_attrs : attrs; (* C of ... [@id1] [@id2] *)
}

let constructor_decl cd_cs cd_ld cd_loc cd_attrs =
  {cd_cs; cd_ld; cd_loc; cd_attrs}

type type_kind =
  | Pty_abstract
  | Pty_variant of constructor_decl list
  | Pty_record of rec_declaration
        (* Invariant: non-empty list *)
  | Pty_open

type private_flag = Private | Public
type variance = Covariant (* + *)
              | Contravariant (* - *)
              | Invariant

(* type (+'a, +'b) t = 'a * 'b
   type (-'a, +'b) t = 'a -> 'b *)

type type_declaration = {
    td_ts         : tysymbol;
    td_params     : (tvsymbol * variance) list;
    (* the core_type in uast can only be Ptyp_var _ or Ptyp_any
       according to the parser *)
    td_cstrs    : (ty * ty * Location.t) list;
    td_kind     : type_kind;
    td_private  : private_flag;
    td_manifest : ty option;
    td_attrs    : attrs;
    td_spec     : type_spec;
    td_loc      : Location.t;
}

let type_declaration td_ts td_params td_cstrs td_kind td_private
      td_manifest td_attrs td_spec td_loc =
  {td_ts; td_params; td_cstrs; td_kind; td_private;
   td_manifest; td_attrs; td_spec; td_loc}

type axiom = {
    ax_name : ident;
    ax_term : term;
    ax_loc  : Location.t;
}

let axiom id t l =
  {ax_name = id; ax_term = t; ax_loc = l}

let mk_axiom id t l =
  t_ty_check t None;
  axiom id t l

type fun_spec = {
  fun_req     : term list;
  fun_ens     : term list;
  fun_variant : term list;
  fun_coer    : bool;
}

let fun_spec req ens var coer =
  {fun_req = req; fun_ens = ens;
   fun_variant = var; fun_coer = coer}

let mk_fun_spec req ens var coer =
  (* check that the requires and ensures are of type
   prop and variants of type integer *)
  let t_ty_check ty t = t_ty_check t ty in
  List.iter (t_ty_check None) req;
  List.iter (t_ty_check None) ens;
  List.iter (t_ty_check (Some ty_integer)) var;
  fun_spec req ens var coer

type function_ = {
    fun_ls     : lsymbol;
    fun_rec    : bool;
    fun_params : vsymbol list;
    fun_def    : term option;
    fun_spec   : fun_spec;
    fun_loc    : Location.t;
}

let function_ ls rec_ params def spec loc =
  {fun_ls = ls; fun_rec = rec_; fun_params = params;
   fun_def = def; fun_spec = spec; fun_loc = loc}

(* For
   (*@ function rec f (x:ty1) (y:ty2):ty3 = t
       variant v
       requires treq
       ensures tens
       coercion
   *)
   we check the following
   1 - no duplicate arguments (identifiers may have the same
   string but still be different)
   2 - types or params match the type of lsymbol
   3 - free variables of t, treq, v come from the arguments;
   in case of tens, if it is a function, it can also be the
   ~result vsylbol
   4 - type of t is ty3 (None if it is a predicate)
   5 - elements of v are of type integer, and elements of treq and
   tens are of type None
*)
let mk_function ?result ls r params def spec loc =

  (* check 1 *)
  let add_v s vs ty =
    check (not (Svs.mem vs s)) (DuplicatedArg vs);
    ty_equal_check vs.vs_ty ty;
    Svs.add vs s in
  let args = List.fold_left2 add_v Svs.empty params ls.ls_args in

  (* check 3 *)
  ignore (opmap (t_free_vs_in_set args) def);
  List.iter (t_free_vs_in_set args) spec.fun_req;
  let args_r = match result,ls.ls_value with
    | Some vs, Some ty ->
       ty_equal_check vs.vs_ty ty; Svs.add vs args
    | _ -> args in
  List.iter (t_free_vs_in_set args_r) spec.fun_ens;

  (* check 4 and 5 *)
  let check_ty ty t = t_ty_check t ty in
  ignore (opmap (check_ty ls.ls_value) def);
  List.iter (check_ty (Some ty_integer)) spec.fun_variant;
  List.iter (check_ty None) spec.fun_ens;

  function_ ls r params def spec loc

type extension_constructor =
    {
     ext_ident     : ident;
     ext_xs        : xsymbol;
     ext_kind      : Oparsetree.extension_constructor_kind;
     ext_loc       : Location.t;
     ext_attributes: Oparsetree.attributes; (* C of ... [@id1] [@id2] *)
   }

let extension_constructor id xs kd loc attrs =
  {ext_ident = id; ext_xs = xs; ext_kind = kd;
   ext_loc = loc; ext_attributes = attrs}

type type_exception =
  {
    exn_constructor : extension_constructor;
    exn_loc         : Location.t;
    exn_attributes  : Oparsetree.attributes; (* ... [@@id1] [@@id2] *)
  }

let type_exception ctr loc attrs =
  {exn_constructor = ctr; exn_loc = loc; exn_attributes = attrs}

type rec_flag = Nonrecursive | Recursive

type ghost = bool

type signature_item_desc =
  | Sig_val of val_description * ghost
  | Sig_type of rec_flag * type_declaration list * ghost
        (* type t1 = ... and ... and tn = ... *)
  | Sig_typext of Oparsetree.type_extension
        (* type t1 += ... *)
  | Sig_module of Uast.s_module_declaration
        (* module X : MT *)
  | Sig_recmodule of Uast.s_module_declaration list
        (* module rec X1 : MT1 and ... and Xn : MTn *)
  | Sig_modtype of Uast.s_module_type_declaration
        (* module type S = MT
           module type S *)
  (* these were not modified *)
  | Sig_exception of type_exception
        (* exception C of T *)
  | Sig_open of Oparsetree.open_description
        (* open X *)
  | Sig_include of Oparsetree.include_description
        (* include MT *)
  | Sig_class of Oparsetree.class_description list
        (* class c1 : ... and ... and cn : ... *)
  | Sig_class_type of Oparsetree.class_type_declaration list
        (* class type ct1 = ... and ... and ctn = ... *)
  | Sig_attribute of Oparsetree.attribute
        (* [@@@id] *)
  | Sig_extension of Oparsetree.extension * Oparsetree.attributes
        (* [%%id] *)
  (* Specific to specification *)
  | Sig_use of string list
  | Sig_function of function_
  | Sig_axiom of axiom

type signature_item = {
    sig_desc: signature_item_desc;
    sig_loc: Location.t;
}

let sig_item sig_desc sig_loc = {sig_desc; sig_loc}

let mk_sig_item desc loc = sig_item desc loc

type signature = signature_item list

(** Pretty printing *)

open Opprintast

let print_variant_field fmt ld =
  pp fmt "%s%a:%a"
    (if ld.ld_mut = Mutable then "mutable " else "")
    print_ident (fst ld.ld_field) print_ty (snd ld.ld_field)

let print_rec_field fmt ld =
  pp fmt "%s%a:%a"
    (if ld.ld_mut = Mutable then "mutable " else "")
    print_ident (ld.ld_field.ls_name)
    print_ty (opget ld.ld_field.ls_value)

let print_label_decl_list print_field fmt fields =
  pp fmt "{%a}"
    (list ~sep:"; " print_field) fields

    (* cd_ld    : (ident * ty) label_declaration list; *)

let print_type_kind fmt = function
  | Pty_abstract -> ()
  | Pty_variant cpl ->
     let print_args cs fmt = function
       | [] -> list ~sep:" * " print_ty fmt cs.ls_args
       | ld -> print_label_decl_list print_variant_field fmt ld in
     let print_constructor fmt {cd_cs;cd_ld} =
       pp fmt "@[%a of %a@\n@[<h 2>%a@]@]"
         print_ident cd_cs.ls_name
         (print_args cd_cs) cd_ld
         print_ls_decl cd_cs in
     pp fmt "@[ = %a@]"
       (list ~sep:"@\n| " print_constructor) cpl
  | Pty_record rd ->
     let pjs = List.map (fun ld -> ld.ld_field) rd.rd_ldl in
     pp fmt "@[ = %a@\n@[<h 2>%a@]@]"
       (print_label_decl_list print_rec_field) rd.rd_ldl
       (list ~sep:"@\n" print_ls_decl) (rd.rd_cs::pjs)
  | Pty_open -> assert false

let print_type_declaration fmt td =
  let print_param fmt (tv,var) =
    let var = match var with
        Covariant -> "+" | Contravariant -> "-" | Invariant -> "" in
   pp fmt "%s%a" var print_tv tv in
  let print_params fmt = function
    | [] -> ()
    | [p] -> pp fmt "%a " print_param p
    | ps -> pp fmt "(%a) " (list ~sep:"," print_param) ps in
  let print_manifest fmt man = match man with
    | None -> ()
    | Some ty -> pp fmt " = %a" print_ty ty in
  let print_constraint fmt (ty1,ty2,_) =
    pp fmt "%a = %a" print_ty ty1 print_ty ty2 in
  pp fmt "@[%a%a%a%a%s%a@]"
    print_params td.td_params
    print_ident (ts_ident td.td_ts)
    print_manifest td.td_manifest
    print_type_kind td.td_kind
    (if td.td_cstrs = [] then "" else " constraint ")
    (list ~sep:" constraint " print_constraint) td.td_cstrs

open Opprintast
open Oparsetree
open Upretty_printer

let print_lb_arg fmt = function
  | Lnone vs -> print_vs fmt vs
  | Lquestion vs -> pp fmt "?%a" print_vs vs
  | Lnamed vs -> pp fmt "~%a" print_vs vs
  | Lghost vs -> pp fmt "[%a: %a]" print_vs vs print_ty vs.vs_ty

let print_xposts f xposts =
  if Mxs.is_empty xposts then () else
  let print xs f (p,t) = pp f "%a@ %a -> %a"
                           print_xs xs print_pattern p print_term t in
  let print_xpost xs = function
    | [] -> pp f "@\n@[<hov 2>@[raises %a@]@]" print_xs xs
    | tl -> list_with_first_last ~first:"@\n@[<hov 2>@[raises "
              ~sep:"@\nraises " ~last:"@]@]"
              (print xs) f tl  in
  Mxs.iter (fun xs tl -> print_xpost xs tl) xposts

let print_vd_spec val_id fmt = function
  | None -> ()
  | Some {sp_args;sp_ret;sp_pre;sp_post;sp_xpost;sp_wr;sp_diverge;sp_equiv} ->
     let pres,checks =
       List.fold_left (fun (pres,checks) (p,c) ->
        if c then pres,p::checks else p::pres,checks) ([],[]) sp_pre in
     pp fmt "(*@ @[%a%s@ %a@ %a@]%a%a%a%a%a%a*)"
       (list ~sep:", " print_lb_arg) sp_ret
       (if sp_ret = [] then "" else " =")
       print_ident val_id
       (list ~sep:" " print_lb_arg) sp_args
    (list_with_first_last ~first:"@\n@[<hov 2>@[requires "
         ~sep:"@\nrequires " ~last:"@]@]"
         print_term) pres
    (list_with_first_last ~first:"@\n@[<hov 2>@[checks "
         ~sep:"@\nchecks " ~last:"@]@]"
         print_term) checks
    (list_with_first_last ~first:"@\n@[<hov 2>@[ensures "
         ~sep:"@\nensures " ~last:"@]@]"
         print_term) sp_post
    print_xposts sp_xpost
    (list_with_first_last ~first:"@\n@[<hov 2>@[writes "
         ~sep:"@\nwrites " ~last:"@]@]"
         print_term) sp_wr
    (list_with_first_last ~first:"@\n@[<hov 2>@[equivalent "
         ~sep:"@\nequivalent " ~last:"@]@]"
         constant_string) sp_equiv

let print_param f p =
  pp f "(%a:%a)" print_ident p.vs_name print_ty p.vs_ty

let print_function f x =
  let func_pred = if x.fun_ls.ls_value = None then "predicate" else "function" in
  let func f x =
    pp f "@[%s %s%a %a%a%a%a%a%a%a@]"
      func_pred
      (if x.fun_rec then "rec " else "")
      print_ident x.fun_ls.ls_name
      (list ~sep:" " print_param) x.fun_params
      (print_option ~first:": " print_ty) x.fun_ls.ls_value
      (print_option ~first:" =@\n@[<hov 2>@["
         ~last:"@]@]" print_term) x.fun_def
      (fun f _ -> if x.fun_spec.fun_coer then pp f "@\ncoercion" else ()) ()
      (list_with_first_last ~first:"@\n@[@[<hov 2>variant "
         ~sep:"@\nvariant " ~last:"@]@]"
         print_term) x.fun_spec.fun_variant
      (list_with_first_last ~first:"@\n@[<hov 2>@[requires "
         ~sep:"@\nrequires " ~last:"@]@]"
         print_term) x.fun_spec.fun_req
      (list_with_first_last ~first:"@\n@[<hov 2>@[ensures "
         ~sep:"@\nensures " ~last:"@]@]"
         print_term) x.fun_spec.fun_ens
  in
  spec func f x

let print_extension_constructor ctxt f x =
  (* Cf: #7200 *)
  match x.ext_kind with
  | Pext_decl (_, _) ->
     print_xs f x.ext_xs
  | Pext_rebind li ->
      pp f "%a%a@;=@;%a" print_ident x.ext_xs.xs_ident
        (attributes ctxt) x.ext_attributes
        longident_loc li

let exception_declaration ctxt f x =
  pp f "@[<hov2>exception@ %a@]%a"
    (print_extension_constructor ctxt) x.exn_constructor
    (item_attributes ctxt) x.exn_attributes

let rec print_signature_item f x =
  match x.sig_desc with
  | Sig_type (rf, td,g) ->
     pp f (if g then "@(*@@@\n[type %a@]@\n*)" else "@[type %a@]")
       (list ~sep:"@\nand " print_type_declaration) td
  | Sig_val (vd,g) ->
      let intro = if vd.vd_prim = [] then "val" else "external" in
      pp f (if g then "@[(*@@@\n@[<2>%s@ %a@ :@ %a %a@]%a@\n%a@\n*)@]"
            else "@[<2>%s@ %a@ :@ %a %a@]%a@\n%a")
        intro
        print_ident vd.vd_name
        core_type vd.vd_type
        (item_attributes reset_ctxt) vd.vd_attrs
        (fun f x ->
          if x.vd_prim <> []
          then pp f "@ =@ %a" (list constant_string) vd.vd_prim
        ) vd
        (print_vd_spec vd.vd_name) vd.vd_spec
  | Sig_typext te ->
      type_extension reset_ctxt f te
  | Sig_exception ed ->
      exception_declaration reset_ctxt f ed
  | Sig_class l ->
      let class_description kwd f ({pci_params=ls;pci_name={txt;_};_} as x) =
        pp f "@[<2>%s %a%a%s@;:@;%a@]%a" kwd
          virtual_flag x.pci_virt
          (class_params_def reset_ctxt) ls txt
          (class_type reset_ctxt) x.pci_expr
          (item_attributes reset_ctxt) x.pci_attributes
      in begin
        match l with
        | [] -> ()
        | [x] -> class_description "class" f x
        | x :: xs ->
            pp f "@[<v>%a@,%a@]"
              (class_description "class") x
              (list ~sep:"@," (class_description "and")) xs
      end
  | Sig_module ({mdtype={mdesc=Mod_alias alias;
                            mattributes=[]; _};_} as pmd) ->
      pp f "@[<hov>module@ %s@ =@ %a@]%a" pmd.mdname.txt
        longident_loc alias
        (item_attributes reset_ctxt) pmd.mdattributes
  | Sig_module pmd ->
      pp f "@[<hov>module@ %s@ :@ %a@]%a"
        pmd.mdname.txt
        print_module_type pmd.mdtype
        (item_attributes reset_ctxt) pmd.mdattributes
  | Sig_open od ->
      pp f "@[<hov2>open%s@ %a@]%a"
        (override od.popen_override)
        longident_loc od.popen_lid
        (item_attributes reset_ctxt) od.popen_attributes
  | Sig_include incl ->
      pp f "@[<hov2>include@ %a@]%a"
        (module_type reset_ctxt) incl.pincl_mod
        (item_attributes reset_ctxt) incl.pincl_attributes
  | Sig_modtype {mtdname=s; mtdtype=md; mtdattributes=attrs} ->
      pp f "@[<hov2>module@ type@ %s%a@]%a"
        s.txt
        (fun f md -> match md with
           | None -> ()
           | Some mt ->
               Format.pp_print_space f () ;
               pp f "@ =@ %a" print_module_type mt
        ) md
        (item_attributes reset_ctxt) attrs
  | Sig_class_type (l) -> class_type_declaration_list reset_ctxt f l
  | Sig_recmodule decls ->
      let rec  string_x_module_type_list f ?(first=true) l =
        match l with
        | [] -> () ;
        | pmd :: tl ->
            if not first then
              pp f "@ @[<hov2>and@ %s:@ %a@]%a" pmd.Uast.mdname.txt
                print_modyle_type1 pmd.mdtype
                (item_attributes reset_ctxt) pmd.mdattributes
            else
              pp f "@[<hov2>module@ rec@ %s:@ %a@]%a" pmd.mdname.txt
                print_modyle_type1 pmd.mdtype
                (item_attributes reset_ctxt) pmd.mdattributes;
            string_x_module_type_list f ~first:false tl
      in
      string_x_module_type_list f decls
  | Sig_attribute a -> floating_attribute reset_ctxt f a
  | Sig_extension(e, a) ->
      item_extension reset_ctxt f e;
      item_attributes reset_ctxt f a
  | Sig_function x -> print_function f x
  | Sig_axiom x -> pp f "(*@@ axiom %a: %a *)"
                     print_ident x.ax_name print_term x.ax_term
  | Sig_use sl -> pp f "(*@@ use %a *)" (list ~sep:"." constant_string) sl

and print_signature f x = list ~sep:"@\n@\n" print_signature_item f x

and print_module_type f x =
  if x.mattributes <> [] then begin
    pp f "((%a)%a)" print_module_type {x with mattributes=[]}
      (attributes reset_ctxt) x.mattributes
  end else
    match x.mdesc with
    | Mod_functor (_, None, mt2) ->
        pp f "@[<hov2>functor () ->@ %a@]" print_module_type mt2
    | Mod_functor (s, Some mt1, mt2) ->
        if s.txt = "_" then
          pp f "@[<hov2>%a@ ->@ %a@]"
            print_modyle_type1 mt1 print_module_type mt2
        else
          pp f "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]" s.txt
            print_module_type mt1 print_module_type mt2
    | Mod_with (mt, []) -> print_module_type f mt
    | Mod_with (mt, l) ->
        let with_constraint f = function
          | Uast.Wtype (li, ({tparams= ls ;_} as td)) ->
              let ls = List.map fst ls in
              pp f "type@ %a %a =@ %a"
                (list core_type ~sep:"," ~first:"(" ~last:")")
                ls longident_loc li s_type_declaration td
          | Wmodule (li, li2) ->
              pp f "module %a =@ %a" longident_loc li longident_loc li2;
          | Wtypesubst (li, ({tparams=ls;_} as td)) ->
              let ls = List.map fst ls in
              pp f "type@ %a %a :=@ %a"
                (list core_type ~sep:"," ~first:"(" ~last:")")
                ls longident_loc li
                s_type_declaration td
          | Wmodsubst (li, li2) ->
             pp f "module %a :=@ %a" longident_loc li longident_loc li2 in
        pp f "@[<hov2>%a@ with@ %a@]"
          print_modyle_type1 mt (list with_constraint ~sep:"@ and@ ") l
    | _ -> print_modyle_type1 f x

and print_modyle_type1 f x =
  if x.mattributes <> [] then print_module_type f x
  else match x.mdesc with
    | Mod_ident li ->
        pp f "%a" longident_loc li;
    | Mod_alias li ->
        pp f "(module %a)" longident_loc li;
    | Mod_signature s ->
        pp f "@[<hv0>@[<hv2>sig@\n%a@]@\nend@]" (* "@[<hov>sig@ %a@ end@]" *)
          (list s_signature_item) s (* FIXME wrong indentation*)
    | Mod_typeof me ->
        pp f "@[<hov2>module@ type@ of@ %a@]" (module_expr reset_ctxt) me
    | Mod_extension e -> extension reset_ctxt f e
    | _ -> paren true print_module_type f x

(** register exceptions *)

let () =
  let open Location in
  register_error_of_exn (function
      | DuplicatedArg vs ->
         Some (errorf ~loc:vs.vs_name.id_loc "Duplicated argument %a" print_vs vs)
      | _ -> None)
