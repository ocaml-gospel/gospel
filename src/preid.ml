(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

let pp_attr ppf attr = Format.fprintf ppf "[@%s]" attr
let pp_attrs = Format.pp_print_list pp_attr

type t = { pid_str : string; pid_attrs : string list; pid_loc : Location.t }

let pp ppf pid =
  let l = String.split_on_char ' ' pid.pid_str in
  match l with
  | [ id ] -> Format.fprintf ppf "%s%a" id pp_attrs pid.pid_attrs
  | [ _; id ] -> Format.fprintf ppf "(%s)%a" id pp_attrs pid.pid_attrs
  | _ -> assert false

let add_attr t attr = { t with pid_attrs = attr :: t.pid_attrs }
let eq id1 id2 = id1.pid_str = id2.pid_str

let create ?(attrs = []) ~loc str =
  { pid_str = str; pid_attrs = attrs; pid_loc = loc }
