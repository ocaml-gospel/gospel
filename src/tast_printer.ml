open Ppxlib
open Utils
open Tast
open Symbols
open Tterm_printer
open Ttypes
open Upretty_printer
open Opprintast
open Fmt

let print_type_spec fmt { ty_ephemeral; ty_fields; ty_invariants } =
  if (not ty_ephemeral) && ty_fields = [] && ty_invariants = [] then ()
  else
    let print_ephemeral f e = if e then pp f "@[ephemeral@]" in
    let print_term f t = pp f "@[%a@]" print_term t in
    let print_field f (ls, mut) =
      pp f "@[%s%a : %a@]"
        (if mut then "mutable model " else "model ")
        print_ls_nm ls print_ty
        (Stdlib.Option.get ls.ls_value)
    in
    pp fmt "(*@@ @[%a%a%a@] *)" print_ephemeral ty_ephemeral
      (list ~first:newline ~sep:newline print_field)
      ty_fields
      (list
         ~first:(newline ++ const string "invariant ")
         ~sep:(const string "invariant") print_term)
      ty_invariants

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

let print_specs ppf =
  object
    inherit Ast_traverse.iter as super

    method! signature_item i =
      Pprintast.signature_item ppf i;
      super#signature_item i
  end

let signature ppf = (print_specs ppf)#signature
