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

type fixity = Normal | Infix | Mixfix | Prefix

type t = {
  pid_str : string;
  pid_fixity : fixity;
  pid_attrs : string list;
  pid_loc : Location.t;
}

let pp ppf pid =
  match pid.pid_fixity with
  | Normal -> Format.fprintf ppf "%s%a" pid.pid_str pp_attrs pid.pid_attrs
  | _ -> Format.fprintf ppf "(%s)%a" pid.pid_str pp_attrs pid.pid_attrs

let add_attr t attr = { t with pid_attrs = attr :: t.pid_attrs }
let eq id1 id2 = id1.pid_str = id2.pid_str

let create ?(fixity = Normal) ?(attrs = []) ~loc str =
  { pid_str = str; pid_fixity = fixity; pid_attrs = attrs; pid_loc = loc }
