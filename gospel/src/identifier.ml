(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Utils

(** Attributes  *)

type attr = string

module Sattr = Set.Make(String)

(** Pre identifiers - not unique - used by parsetree and untyped AST *)

type preid = {
  pid_str : string;
  pid_ats : attr list;
  pid_loc : Location.t;
}

module Preid = struct
  type t = preid
  let equal = (=)
  let hash = (Hashtbl.hash : preid -> int)
end

module Hpid  = Hashtbl.Make(Preid)

let create_pid s a l = {pid_str = s; pid_ats = a; pid_loc = l}

let pid_of_string s = create_pid s [] Location.none

let pid_add_lab pid l = { pid with pid_ats =  pid.pid_ats @ l }

(** Identifiers *)

type ident = {
  id_str : string;
  id_ats : Sattr.t;
  id_loc : Location.t;
  id_tag : int;
}

module Ident = struct
  type t = ident
  let compare = Stdlib.compare
  let equal = (=)
  let hash = (Hashtbl.hash : ident -> int)
end

module Hid = Hashtbl.Make(Ident)
module Mid = Map.Make(Ident)

let create_id =
  let r = ref 0 in
  fun s a l -> {
      id_str = s;
      id_ats = a;
      id_loc = l;
      id_tag = (incr r; !r)
    }

let id_register pid =
  create_id pid.pid_str (Sattr.of_list pid.pid_ats) pid.pid_loc

let fresh_id ?loc ?ats s =
  let loc = Option.value loc ~default:Location.none in
  let ats = Option.value ats ~default:Sattr.empty in
  create_id s ats loc

let id_add_loc l id = {id with id_loc = l}

let id_add_lab id l = { id with id_ats = Sattr.add l id.id_ats }

(* utils *)

let prefix s = "prefix " ^ s
let infix  s = "infix "  ^ s
let mixfix s = "mixfix " ^ s

let is_somefix f s =
  let sl = String.split_on_char ' ' s in
  List.length sl > 1 && List.hd sl = f

let is_prefix = is_somefix "prefix"
let is_infix  = is_somefix "infix"
let is_mixfix = is_somefix "mixfix"

(* hard-coded ids *)

let eq    = fresh_id (infix "=")
let neq   = fresh_id (infix "<>")
let none  = fresh_id ("None")
let some  = fresh_id ("Some")
let nil   = fresh_id ("[]")
let cons  = fresh_id (infix "::")

(* pretty-printer *)

open Opprintast

let print_attr fmt a = pp fmt "[@%s]" a
let print_attrs = list ~sep:" " print_attr

let print_pid fmt pid = pp fmt "%s@ %a" pid.pid_str
                            print_attrs pid.pid_ats

let print_ident =
  let current = Hashtbl.create 0 in
  let output  = Hashtbl.create 0 in
  let current s =
    let x = match Hashtbl.find_opt current s with
      | Some x -> x + 1
      | None -> 0
    in
    Hashtbl.replace current s x; x
  in
  let str_of_id id =
    try Hashtbl.find output id.id_tag with
    | Not_found ->
       let x = current id.id_str in
       let str = if x = 0 then id.id_str else
                   id.id_str ^ "#" ^ string_of_int x in
       Hashtbl.replace output id.id_tag str; str in
  fun fmt id -> pp fmt "%s%a" (str_of_id id)
                  print_attrs (Sattr.elements id.id_ats)
