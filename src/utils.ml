(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

let rec split_at_f f = function
  | [] -> ([], [])
  | x :: xs when f x ->
      let xs', ys' = split_at_f f xs in
      (x :: xs', ys')
  | l -> ([], l)

let rec split_at_i i = function
  | [] -> ([], [])
  | l when i <= 0 -> ([], l)
  | x :: xs ->
      let xs', ys' = split_at_i (pred i) xs in
      (x :: xs', ys')

module Fmt = struct
  include Fmt

  let list ?(first = nop) ?(last = nop) ?sep pp_v ppf l =
    if List.length l = 0 then ()
    else pf ppf "%a@[%a@]%a" first () (list ?sep pp_v) l last ()

  let pp = pf
  let full ppf _ = pf ppf ".@ "
  let arrow ppf _ = pf ppf " ->@ "
  let star ppf _ = pf ppf " *@ "
  let newline ppf _ = pf ppf "@\n"
  let lparens ppf _ = pf ppf "@[<1>("
  let rparens ppf _ = pf ppf ")@]"
  let lbracket ppf _ = pf ppf "@[<1>["
  let rbracket ppf _ = pf ppf "]@]"
  let lbrace ppf _ = pf ppf "@[<1>{"
  let rbrace ppf _ = pf ppf "}@]"

  let pp_loc ppf loc =
    let open Ppxlib.Location in
    let s = loc.loc_start in
    if s.pos_fname = "_none_" then pf ppf "none"
    else pf ppf "%s:%d:%d" s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol)
end

module Sstr = Set.Make (String)
