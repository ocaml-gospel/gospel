open Utils
open Ppxlib
open Tast
open Symbols
open Tterm_printer
open Ttypes
open Upretty_printer
open Opprintast
open Fmt
module Option = Stdlib.Option

let print_variant_field fmt ld =
  pp fmt "%s%a:%a"
    (if ld.ld_mut = Mutable then "mutable " else "")
    Ident.pp (fst ld.ld_field) print_ty (snd ld.ld_field)

let print_rec_field fmt ld =
  pp fmt "%s%a:%a"
    (if ld.ld_mut = Mutable then "mutable " else "")
    Ident.pp ld.ld_field.ls_name print_ty
    (Stdlib.Option.get ld.ld_field.ls_value)

let print_label_decl_list print_field fmt fields =
  pp fmt "{%a}" (list ~sep:semi print_field) fields

let print_type_kind fmt = function
  | Pty_abstract -> ()
  | Pty_variant cpl ->
      let print_args cs fmt = function
        | [] -> list ~sep:star print_ty fmt cs.ls_args
        | ld -> print_label_decl_list print_variant_field fmt ld
      in
      let print_constructor fmt { cd_cs; cd_ld; _ } =
        pp fmt "@[%a of %a@\n@[<h 2>%a@]@]" Ident.pp cd_cs.ls_name
          (print_args cd_cs) cd_ld print_ls_decl cd_cs
      in
      pp fmt "@[ = %a@]"
        (list ~sep:(newline ++ const string "| ") print_constructor)
        cpl
  | Pty_record rd ->
      let pjs = List.map (fun ld -> ld.ld_field) rd.rd_ldl in
      pp fmt "@[ = %a@\n@[<h 2>%a@]@]"
        (print_label_decl_list print_rec_field)
        rd.rd_ldl
        (list ~sep:newline print_ls_decl)
        (rd.rd_cs :: pjs)

let print_type_spec fmt { ty_ephemeral; ty_fields; ty_invariants; _ } =
  if (not ty_ephemeral) && ty_fields = [] && Option.is_none ty_invariants then
    ()
  else
    let print_ephemeral f e = if e then pp f "@[ephemeral@]" in
    let print_term f t = pp f "@[%a@]" print_term t in
    let print_field f (ls, mut) =
      pp f "@[%s%a : %a@]"
        (if mut then "mutable model " else "model ")
        print_ls_nm ls print_ty
        (Stdlib.Option.get ls.ls_value)
    in
    let print_invariants ppf i =
      pf ppf "with %a@;%a" print_vs (fst i)
        (list
           ~first:(newline ++ const string "invariant ")
           ~sep:(const string "@\ninvariant")
           print_term)
        (snd i)
    in
    pp fmt "(*@@ @[%a%a%a@] *)" print_ephemeral ty_ephemeral
      (list ~first:newline ~sep:newline print_field)
      ty_fields
      (* (option print_invariant_vs) *)
      (* ty_invariants *)
      (option print_invariants)
      ty_invariants

let print_type_declaration fmt td =
  let print_param fmt (tv, (v, i)) =
    pp fmt "%s%s%a" (type_variance v) (type_injectivity i) print_tv tv
  in
  let print_params fmt = function
    | [] -> ()
    | [ p ] -> pp fmt "%a " print_param p
    | ps -> pp fmt "(%a) " (list ~sep:comma print_param) ps
  in
  let print_manifest fmt man =
    match man with None -> () | Some ty -> pp fmt " = %a" print_ty ty
  in
  let print_constraint fmt (ty1, ty2, _) =
    pp fmt "%a = %a" print_ty ty1 print_ty ty2
  in
  pp fmt "@[%a%a%a%a%s%a@]@\n@[%a@]" print_params td.td_params Ident.pp
    (ts_ident td.td_ts) print_manifest td.td_manifest print_type_kind td.td_kind
    (if td.td_cstrs = [] then "" else " constraint ")
    (list ~sep:(const string " constraint ") print_constraint)
    td.td_cstrs (option print_type_spec) td.td_spec

let print_lb_arg fmt = function
  | Lunit -> pp fmt "()"
  | Lnone vs -> print_vs fmt vs
  | Loptional vs -> pp fmt "?%a" print_vs vs
  | Lnamed vs -> pp fmt "~%a" print_vs vs
  | Lghost vs -> pp fmt "[%a: %a]" print_vs vs print_ty vs.vs_ty

let print_xposts f xposts =
  if xposts = [] then ()
  else
    let print xs f (p, t) =
      pp f "@[@[%a@ %a@] -> @[%a@]@]" print_xs xs print_pattern p print_term t
    in
    let print_xpost (xs, tl) =
      match tl with
      | [] -> pp f "@\n@[raises %a@]" print_xs xs
      | tl ->
          list
            ~first:(newline ++ const string "raises")
            ~sep:(sp ++ const string "| ")
            (print xs) f tl
    in
    List.iter print_xpost xposts

let print_vd_spec val_id fmt spec =
  let print_term f t = pp f "@[%a@]" print_term t in
  let print_diverges f d = if not d then () else pp f "@\n@[diverges@]" in
  match spec with
  | None -> ()
  | Some vs ->
      pp fmt "(*@@ @[%a%s@ %a@ %a@]%a%a%a%a%a%a%a%a*)"
        (list ~sep:comma print_lb_arg)
        vs.sp_ret
        (if vs.sp_ret = [] then "" else " =")
        Ident.pp val_id
        (list ~sep:sp print_lb_arg)
        vs.sp_args print_diverges vs.sp_diverge
        (list
           ~first:(newline ++ const string "requires ")
           ~sep:(newline ++ const string "requires ")
           print_term)
        vs.sp_pre
        (list
           ~first:(newline ++ const string "checks ")
           ~sep:(newline ++ const string "checks ")
           print_term)
        vs.sp_checks
        (list
           ~first:(newline ++ const string "ensures ")
           ~sep:(newline ++ const string "ensures ")
           print_term)
        vs.sp_post print_xposts vs.sp_xpost
        (list
           ~first:(newline ++ const string "writes ")
           ~sep:(newline ++ const string "writes ")
           print_term)
        vs.sp_wr
        (list
           ~first:(newline ++ const string "consumes ")
           ~sep:(newline ++ const string "consumes ")
           print_term)
        vs.sp_cs
        (list
           ~first:(newline ++ const string "equivalent ")
           ~sep:(newline ++ const string "equivalent ")
           constant_string)
        vs.sp_equiv

let print_param f p = pp f "(%a:%a)" Ident.pp p.vs_name print_ty p.vs_ty

let print_function f x =
  let func_pred =
    if x.fun_ls.ls_value = None then "predicate" else "function"
  in
  let print_term f t = pp f "@[%a@]" print_term t in
  let print_term f t = pp f "@[%a@]" print_term t in
  let func_spec f x =
    pp f "%a%a%a%a"
      (fun f _ -> if x.fun_coer then pp f "@\ncoercion" else ())
      ()
      (list
         ~first:(newline ++ const string "variant ")
         ~sep:(newline ++ const string "variant ")
         print_term)
      x.fun_variant
      (list
         ~first:(newline ++ const string "requires ")
         ~sep:(newline ++ const string "requires ")
         print_term)
      x.fun_req
      (list
         ~first:(newline ++ const string "ensures ")
         ~sep:(newline ++ const string "ensures ")
         print_term)
      x.fun_ens
  in
  let func f x =
    pp f "@[%s %s%a %a%a%a%a@]" func_pred
      (if x.fun_rec then "rec " else "")
      Ident.pp x.fun_ls.ls_name (list ~sep:sp print_param) x.fun_params
      (option (fun f -> pp f ": %a" print_ty))
      x.fun_ls.ls_value
      (option (fun f -> pp f " =@\n@[<hov2>@[%a@]@]" print_term))
      x.fun_def (option func_spec) x.fun_spec
  in
  spec func f x

let print_extension_constructor ctxt f x =
  (* Cf: #7200 *)
  match x.ext_kind with
  | Pext_decl (_, _, _) -> print_xs f x.ext_xs
  | Pext_rebind li ->
      pp f "%a%a@;=@;%a" Ident.pp x.ext_xs.xs_ident (attributes ctxt)
        x.ext_attributes longident_loc li

let exception_declaration ctxt f x =
  pp f "@[<hov2>exception@ %a@]%a"
    (print_extension_constructor ctxt)
    x.exn_constructor (item_attributes ctxt) x.exn_attributes

let rec print_signature_item f x =
  let print_val f vd =
    let intro = if vd.vd_prim = [] then "val" else "external" in
    pp f "@[%s@ %a@ :@ %a%a%a@]@\n@[<h4>%a@]" intro Ident.pp vd.vd_name
      core_type vd.vd_type
      (fun f x ->
        if x.vd_prim <> [] then pp f "@ =@ %a" (list constant_string) x.vd_prim)
      vd
      (item_attributes reset_ctxt)
      vd.vd_attrs (print_vd_spec vd.vd_name) vd.vd_spec
  in
  match x.sig_desc with
  | Sig_type (_, td, g) ->
      pp f
        (if g = Ghost then "@[(*@@ type %a *)@]" else "@[type %a@]")
        (list ~sep:(newline ++ const string "and ") print_type_declaration)
        td
  | Sig_val (vd, g) ->
      pp f (if g = Ghost then "@[(*@@@ %a@ *)@]" else "@[%a@]") print_val vd
  | Sig_typext te -> type_extension reset_ctxt f te
  | Sig_exception ed -> exception_declaration reset_ctxt f ed
  | Sig_class l -> (
      let class_description kwd f
          ({ pci_params = ls; pci_name = { txt; _ }; _ } as x) =
        pp f "@[<2>%s %a%a%s@;:@;%a@]%a" kwd virtual_flag x.pci_virt
          (class_params_def reset_ctxt)
          ls txt class_type x.pci_expr
          (item_attributes reset_ctxt)
          x.pci_attributes
      in
      match l with
      | [] -> ()
      | [ x ] -> class_description "class" f x
      | x :: xs ->
          pp f "@[<v>%a@,%a@]"
            (class_description "class")
            x
            (list ~sep:comma (class_description "and"))
            xs)
  | Sig_module
      ({ md_type = { mt_desc = Mod_alias alias; mt_attrs = []; _ }; _ } as pmd)
    ->
      pp f "@[<hov>module@ %a@ =@ %a@]%a" Ident.pp pmd.md_name
        (list ~sep:full Format.pp_print_string)
        alias
        (item_attributes reset_ctxt)
        pmd.md_attrs
  | Sig_module pmd ->
      pp f "@[<hov>module@ %a@ :@ %a@]%a" Ident.pp pmd.md_name print_module_type
        pmd.md_type
        (item_attributes reset_ctxt)
        pmd.md_attrs
  | Sig_open (od, ghost) ->
      pp f
        (if ghost = Ghost then "@[<hov2>(*@@@ open%s@ %a@ *)@]%a"
         else "@[<hov2>open%s@ %a@]%a")
        (override od.opn_override)
        (list ~sep:full Format.pp_print_string)
        od.opn_id
        (item_attributes reset_ctxt)
        od.opn_attrs
  | Sig_include incl ->
      pp f "@[<hov2>include@ %a@]%a" module_type incl.pincl_mod
        (item_attributes reset_ctxt)
        incl.pincl_attributes
  | Sig_modtype { mtd_name = s; mtd_type = md; mtd_attrs = attrs; _ } ->
      pp f "@[<hov2>module@ type@ %a%a@]%a" Ident.pp s
        (fun f md ->
          match md with
          | None -> ()
          | Some mt ->
              Format.pp_print_space f ();
              pp f "@ =@ %a" print_module_type mt)
        md
        (item_attributes reset_ctxt)
        attrs
  | Sig_class_type l -> class_type_declaration_list reset_ctxt f l
  (* | Sig_recmodule decls ->
   *     let rec  string_x_module_type_list f ?(first=true) l =
   *       match l with
   *       | [] -> () ;
   *       | pmd :: tl ->
   *           if not first then
   *             pp f "@ @[<hov2>and@ %a:@ %a@]%a" Ident.pp pmd.mdname
   *               print_modyle_type1 pmd.mdtype
   *               (item_attributes reset_ctxt) pmd.mdattributes
   *           else
   *             pp f "@[<hov2>module@ rec@ %a:@ %a@]%a" Ident.pp pmd.mdname
   *               print_modyle_type1 pmd.mdtype
   *               (item_attributes reset_ctxt) pmd.mdattributes;
   *           string_x_module_type_list f ~first:false tl
   *     in
   *     string_x_module_type_list f decls *)
  | Sig_attribute a -> floating_attribute reset_ctxt f a
  | Sig_extension (e, a) ->
      item_extension reset_ctxt f e;
      item_attributes reset_ctxt f a
  | Sig_function x -> print_function f x
  | Sig_axiom x ->
      pp f "(*@@ axiom %a: %a *)" Ident.pp x.ax_name print_term x.ax_term
  | Sig_use s -> pp f "(*@@ use %s *)" s
  | _ -> assert false

and print_signature f x =
  list ~sep:(newline ++ newline) print_signature_item f x

and print_module_type f x =
  if x.mt_attrs <> [] then
    pp f "((%a)%a)" print_module_type { x with mt_attrs = [] }
      (attributes reset_ctxt) x.mt_attrs
  else
    match x.mt_desc with
    | Mod_functor (_, None, mt2) ->
        pp f "@[<hov2>functor () ->@ %a@]" print_module_type mt2
    | Mod_functor (s, Some mt1, mt2) ->
        if s.id_str = "_" then
          pp f "@[<hov2>%a@ ->@ %a@]" print_modyle_type1 mt1 print_module_type
            mt2
        else
          pp f "@[<hov2>functor@ (%a@ :@ %a)@ ->@ %a@]" Ident.pp s
            print_module_type mt1 print_module_type mt2
    | Mod_with (mt, []) -> print_module_type f mt
    | Mod_with (mt, l) ->
        let with_constraint f = function
          | Wty (li, td) ->
              (* let ls = List.map fst td.td_params in *)
              (* small hack to print the original id *)
              let ts = { td.td_ts with ts_ident = li } in
              let td = { td with td_ts = ts } in
              pp f "type@ %a"
                (* (list print_tv ~sep:comma ~first:lparens ~last:rparens) ls
                 * Ident.pp li *)
                print_type_declaration td
          | Wmod (li, li2) -> pp f "module %a =@ %a" Ident.pp li Ident.pp li2
          | Wtysubs (li, ({ td_params = ls; _ } as td)) ->
              let ls = List.map fst ls in
              let ts = { td.td_ts with ts_ident = li } in
              let td = { td with td_ts = ts } in
              pp f "type@ %a %a :=@ %a"
                (list print_tv ~sep:comma ~first:lparens ~last:rparens)
                ls Ident.pp li print_type_declaration td
          | Wmodsubs (li, li2) ->
              pp f "module %a :=@ %a" Ident.pp li Ident.pp li2
        in
        pp f "@[<hov2>%a@ with@ %a@]" print_modyle_type1 mt
          (list with_constraint ~sep:(any " and@ "))
          l
    | _ -> print_modyle_type1 f x

and print_modyle_type1 f x =
  if x.mt_attrs <> [] then print_module_type f x
  else
    match x.mt_desc with
    | Mod_ident li -> pp f "%a" (list ~sep:full Format.pp_print_string) li
    | Mod_alias li ->
        pp f "(module %a)" (list ~sep:full Format.pp_print_string) li
    | Mod_signature s ->
        pp f "@[<hv0>@[<hv2>sig@\n%a@]@\nend@]" (* "@[<hov>sig@ %a@ end@]" *)
          (list ~sep:newline print_signature_item)
          s
        (* FIXME wrong indentation*)
    | Mod_typeof me -> pp f "@[<hov2>module@ type@ of@ %a@]" module_expr me
    | Mod_extension e -> extension reset_ctxt f e
    | _ -> paren true print_module_type f x
