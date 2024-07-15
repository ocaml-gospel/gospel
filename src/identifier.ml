(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ppxlib

let pp_attr ppf attr = Format.fprintf ppf "[@%s]" attr
let pp_attrs = Format.pp_print_list pp_attr

type fixity = Prefix | Infix | Mixfix | Normal

module Preid = struct
  type t = {
    pid_str : string;
    pid_fixity : fixity;
    pid_attrs : string list;
    pid_loc : Location.t;
  }

  let pp ppf pid = Format.fprintf ppf "%s%a" pid.pid_str pp_attrs pid.pid_attrs

  let create ?(fixity = Normal) ?(attrs = []) ~loc str =
    { pid_str = str; pid_fixity = fixity; pid_attrs = attrs; pid_loc = loc }

  let add_attr t attr = { t with pid_attrs = attr :: t.pid_attrs }
end

module Ident = struct
  type t = {
    id_str : string;
    id_fixity : fixity;
    id_attrs : string list;
    id_path : string list;
    id_loc : Location.t;
    id_tag : int;
  }

  let pp =
    let current = Hashtbl.create 0 in
    let output = Hashtbl.create 0 in
    let current s =
      let x = Hashtbl.find_opt current s |> Option.fold ~none:0 ~some:succ in
      Hashtbl.replace current s x;
      x
    in
    let str_of_id path id =
      try Hashtbl.find output id.id_tag
      with Not_found ->
        let x = current id.id_str in
        let str =
          if x = 0 then id.id_str else id.id_str ^ "_" ^ string_of_int x
        in
        let str =
          if path then
            List.fold_right (fun e acc -> e ^ "." ^ acc) id.id_path str
          else str
        in
        Hashtbl.replace output id.id_tag str;
        str
    in
    fun path ppf t ->
      Format.fprintf ppf "%s%a" (str_of_id path t) pp_attrs t.id_attrs

  let pp_simpl = pp false
  let pp = pp true

  let create =
    let tag = ref 0 in
    fun ?(fixity = Normal) ?(attrs = []) ?(path = []) ~loc str ->
      incr tag;
      {
        id_str = str;
        id_fixity = fixity;
        id_attrs = attrs;
        id_path = path;
        id_loc = loc;
        id_tag = !tag;
      }

  let of_preid ?(path = []) (pid : Preid.t) =
    create pid.pid_str ~fixity:pid.pid_fixity ~path ~attrs:pid.pid_attrs
      ~loc:pid.pid_loc

  let set_loc t loc = { t with id_loc = loc }
  let add_attr t attr = { t with id_attrs = attr :: t.id_attrs }
  let compare x y = Int.compare x.id_tag y.id_tag
  let equal x y = x.id_tag = y.id_tag
  let hash x = x.id_tag
end

let is_somefix f s =
  let sl = String.split_on_char ' ' s in
  List.length sl > 1 && List.hd sl = f

let is_prefix = is_somefix "prefix"
let is_infix = is_somefix "infix"
let is_mixfix = is_somefix "mixfix"
let eq = Ident.create ~fixity:Infix ~loc:Location.none "="
let neq = Ident.create ~fixity:Infix ~loc:Location.none "<>"
let none = Ident.create ~loc:Location.none "None"
let some = Ident.create ~loc:Location.none "Some"
let nil = Ident.create ~loc:Location.none "[]"
let cons = Ident.create ~fixity:Infix ~loc:Location.none "::"
