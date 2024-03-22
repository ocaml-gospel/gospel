(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Tterm
open Symbols
open Ttypes
open Utils
open Fmt

let print_vs fmt { vs_name; vs_ty } =
  pp fmt "@[%a:%a@]" Ident.pp vs_name print_ty vs_ty

let print_ls_decl fmt { ls_name; ls_args; ls_value; _ } =
  let is_func = Option.is_some ls_value in
  let print_unnamed_arg fmt ty = pp fmt "(_:%a)" print_ty ty in
  pp fmt "%s %a %a%s%a"
    (if is_func then "function" else "predicate")
    Ident.pp ls_name
    (list ~sep:sp print_unnamed_arg)
    ls_args
    (if is_func then " : " else "")
    (option print_ty) ls_value

let print_ls_nm fmt { ls_name; _ } = pp fmt "%a" Ident.pp ls_name
let protect_on x s = if x then "(" ^^ s ^^ ")" else s

let rec print_pat_node pri fmt p =
  match p.p_node with
  | Pwild -> pp fmt "_"
  | Pvar v -> print_vs fmt v
  | Pas (p, v) ->
      pp fmt (protect_on (pri > 1) "%a as %a") (print_pat_node 1) p print_vs v
  | Por (p, q) ->
      pp fmt
        (protect_on (pri > 0) "%a | %a")
        (print_pat_node 0) p (print_pat_node 0) q
  | Papp (cs, pl) when is_fs_tuple cs ->
      pp fmt (protect_on (pri > 0) "%a") (list ~sep:comma (print_pat_node 1)) pl
  | Papp (cs, []) -> print_ls_nm fmt cs
  | Papp (cs, [ pl ]) -> pp fmt "%a@ %a" print_ls_nm cs (print_pat_node 2) pl
  | Papp (cs, pl) ->
      pp fmt "%a@ (%a)" print_ls_nm cs (list ~sep:comma (print_pat_node 2)) pl
  | Pconst c -> Opprintast.constant fmt c
  | Pinterval (c1, c2) -> pp fmt "%C..%C" c1 c2

let print_pattern = print_pat_node 0

let print_binop fmt = function
  | Tand -> pp fmt "/\\"
  | Tor -> pp fmt "\\/"
  | Timplies -> pp fmt "->"
  | Tiff -> pp fmt "<->"
  | Tand_asym -> pp fmt "&&"
  | Tor_asym -> pp fmt "||"

let print_quantifier fmt = function
  | Tforall -> pp fmt "forall"
  | Texists -> pp fmt "exists"

(* TODO use pretty printer from why3 *)
let rec print_term fmt { t_node; t_ty; t_attrs; _ } =
  let print_ty fmt ty =
    match ty with None -> pp fmt ":prop" | Some ty -> pp fmt ":%a" print_ty ty
  in
  let print_t_node fmt t_node =
    match t_node with
    | Tconst c -> pp fmt "%a%a" Opprintast.constant c print_ty t_ty
    | Ttrue -> pp fmt "true%a" print_ty t_ty
    | Tfalse -> pp fmt "false%a" print_ty t_ty
    | Tvar vs ->
        pp fmt "%a" print_vs vs;
        assert (vs.vs_ty = Option.get t_ty) (* TODO remove this *)
    | Tapp (ls, [ x1; x2 ]) when Identifier.is_infix ls.ls_name.id_str ->
        let op_nm =
          match String.split_on_char ' ' ls.ls_name.id_str with
          | [ x ] | [ _; x ] -> x
          | _ -> assert false
        in
        pp fmt "(%a %s %a)%a" print_term x1 op_nm print_term x2 print_ty t_ty
    | Tapp (ls, tl) ->
        pp fmt "(%a %a)%a" Ident.pp ls.ls_name
          (list ~first:sp ~sep:sp print_term)
          tl print_ty t_ty
    | Tfield (t, ls) -> pp fmt "(%a).%a" print_term t Ident.pp ls.ls_name
    | Tnot t -> pp fmt "not %a" print_term t
    | Tif (t1, t2, t3) ->
        pp fmt "if %a then %a else %a" print_term t1 print_term t2 print_term t3
    | Tlet (vs, t1, t2) ->
        pp fmt "let %a = %a in %a" print_vs vs print_term t1 print_term t2
    | Tbinop (op, t1, t2) ->
        pp fmt "%a %a %a" print_term t1 print_binop op print_term t2
    | Tquant (q, vsl, t) ->
        pp fmt "%a %a. %a" print_quantifier q (list ~sep:sp print_vs) vsl
          print_term t
    | Tlambda (pl, t) ->
        pp fmt "fun %a -> %a" (list ~sep:sp print_pattern) pl print_term t
    | Tcase (t, ptl) ->
        let print_branch fmt (p, g, t) =
          match g with
          | None -> pp fmt "| @[%a@] -> @[%a@]" print_pattern p print_term t
          | Some g ->
              pp fmt "| @[%a@] when @[%a@] -> @[%a@]" print_pattern p print_term
                g print_term t
        in
        pp fmt "match %a with@\n%a@\nend:%a" print_term t
          (list ~sep:newline print_branch)
          ptl print_ty t_ty
    | Told t -> pp fmt "old (%a)" print_term t
  in
  let print_attrs fmt = List.iter (pp fmt "[%@ %s]") in
  pp fmt "%a%a" print_attrs t_attrs print_t_node t_node
