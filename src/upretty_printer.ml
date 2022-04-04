(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Uast
open Utils.Fmt

let const_hole s fmt _ = pp fmt "%s" s

let rec qualid fmt (q : qualid) =
  match q with
  | Qpreid pid -> Preid.pp fmt pid
  | Qdot (q, pid) -> pp fmt "@[%a.%a@]" qualid q Preid.pp pid

let labelled_arg fmt (l : labelled_arg) =
  match l with
  | Lunit -> pp fmt "()"
  | Lnone pid -> Preid.pp fmt pid
  | Loptional pid -> pp fmt "@[?%a@]" Preid.pp pid
  | Lnamed pid -> pp fmt "@[~%a@]" Preid.pp pid
  | Lghost (pid, _) -> pp fmt "@[[%a : TY]@]" Preid.pp pid

let spec f fmt x = pp fmt "@[(*@@ %a@ *)@]" f x
let term fmt _ = pp fmt "@[TERM ... @]"
let invariant fmt _ = pp fmt "@[INVARIANT ... @]"

let list_keyword s fmt x =
  match x with
  | [] -> ()
  | _ -> pp fmt "%a@\n" (list ~sep:newline (const_hole s)) x

let type_spec f ts =
  let ephemeral f e = if e then pp f "ephemeral@\n" else () in
  let print_tspec _fmt ts =
    pp f "@[<v>%a%a%a@]" ephemeral ts.ty_ephemeral (list_keyword "model ...")
      ts.ty_field
      (list_keyword "invariant ...")
      ts.ty_invariant
  in
  if ts.ty_ephemeral || ts.ty_field != [] || ts.ty_invariant != [] then
    pp f "@[%a@]" (spec print_tspec) ts
  else ()

let spec_header fmt h =
  pp fmt "@[<h>%a%s %a %a@]@\n"
    (list ~sep:comma labelled_arg)
    h.sp_hd_ret
    (if h.sp_hd_ret = [] then "" else " =")
    Preid.pp h.sp_hd_nm
    (list ~sep:sp labelled_arg)
    h.sp_hd_args

let val_spec fmt vspec =
  match vspec with
  | None -> ()
  | Some vspec ->
      let diverge fmt x = if x then pp fmt "diverges@\n" else () in
      let print_content fmt s =
        pp fmt "@[%a%a%a%a%a%a%a%a@]" (option spec_header) s.sp_header
          (list_keyword "requires ...")
          s.sp_pre
          (list_keyword "ensures ...")
          s.sp_post (list_keyword "with ...") s.sp_xpost
          (list_keyword "modifies ...")
          s.sp_writes
          (list_keyword "consumes ...")
          s.sp_consumes diverge s.sp_diverge
          (list_keyword "equivalent ...")
          s.sp_equiv
      in
      spec print_content fmt vspec

let function_ f x =
  let keyword =
    match x.fun_type with None -> "predicate" | Some _ -> "function"
  in
  let sep f x =
    match x with
    | { fun_req = []; fun_ens = []; fun_variant = []; _ } -> ()
    | _ -> pp f "@\n"
  in
  let func_spec f x =
    pp f "%a%a%a%a%a"
      (fun f _ -> if x.fun_coer then pp f "@\ncoercion" else ())
      () sep x
      (list_keyword "variant ...")
      x.fun_variant
      (list_keyword "requires ...")
      x.fun_req
      (list_keyword "ensures ...")
      x.fun_ens
  in
  let func f x =
    pp f "@[%s %s%a ...%a@]" keyword
      (if x.fun_rec then "rec " else "")
      Preid.pp x.fun_name (option func_spec) x.fun_spec
  in
  spec func f x

let axiom f x =
  let axiom f _ = pp f "@[axiom ...@]" in
  spec axiom f x
