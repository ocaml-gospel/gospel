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

module Preid = struct
  type t = { pid_str : string; pid_loc : Location.t }

  let pp ppf pid = Format.fprintf ppf "%s" pid.pid_str
  let create ~loc str = { pid_str = str; pid_loc = loc }
end

module Ident = struct
  type t = {
    id_str : string;
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
    fun path ppf t -> Format.fprintf ppf "%s" (str_of_id path t)

  let pp_simpl = pp false
  let pp = pp true

  let create =
    let tag = ref 0 in
    fun ?(path = []) ~loc str ->
      incr tag;
      { id_str = str; id_path = path; id_loc = loc; id_tag = !tag }

  let of_preid ?(path = []) (pid : Preid.t) =
    create pid.pid_str ~path ~loc:pid.pid_loc

  let set_loc t loc = { t with id_loc = loc }
  let compare x y = Int.compare x.id_tag y.id_tag
  let equal x y = x.id_tag = y.id_tag
  let hash x = x.id_tag
end

let prefix s = "prefix " ^ s
let infix s = "infix " ^ s
let mixfix s = "mixfix " ^ s

let is_somefix f s =
  let sl = String.split_on_char ' ' s in
  List.length sl > 1 && List.hd sl = f

let is_prefix = is_somefix "prefix"
let is_infix = is_somefix "infix"
let is_mixfix = is_somefix "mixfix"
let eq = Ident.create ~loc:Location.none (infix "=")
let neq = Ident.create ~loc:Location.none (infix "<>")
let none = Ident.create ~loc:Location.none "None"
let some = Ident.create ~loc:Location.none "Some"
let nil = Ident.create ~loc:Location.none "[]"
let cons = Ident.create ~loc:Location.none (infix "::")
