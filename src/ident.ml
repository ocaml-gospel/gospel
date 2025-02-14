(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Definition of program variables. *)

type t = {
  id_str : string; (* Variable name. Not used internally. *)
  id_attrs : string list; (* Variable attributes *)
  id_loc : Location.t;
  id_local : bool;
  (* This variable is used to check if the variable is defined outside
     the scope of the top level definition it is contained in. This is
     marked as [true] for variables defined in [let] bindings or
     quantifiers. This is also true for identifiers for recursive top
     level definitions within the scope of the definition. *)
  id_tag : int;
      (* Unique identifier. During typechecking, this is what
       Inferno uses to check if two variables are the same. *)
}

let pp ppf id =
  let pp_attr ppf attr = Format.fprintf ppf "[@%s]" attr in
  let pp_attrs = Format.pp_print_list pp_attr in
  let l = String.split_on_char ' ' id.id_str in
  match l with
  | [ v ] -> Format.fprintf ppf "%s%a" v pp_attrs id.id_attrs
  | [ _; v ] -> Format.fprintf ppf "(%s)%a" v pp_attrs id.id_attrs
  | _ -> assert false

let to_string id = id.id_str
let compare x y = Int.compare x.id_tag y.id_tag
let equal x y = x.id_tag = y.id_tag
let hash x = x.id_tag

let gen_tag =
  let r = ref 0 in
  fun () ->
    r := !r + 1;
    !r

let mk_id id_str id_loc =
  { id_str; id_attrs = []; id_loc; id_local = false; id_tag = gen_tag () }

let tvar () =
  {
    id_str = "a";
    id_attrs = [];
    id_loc = Location.none;
    id_local = true;
    id_tag = gen_tag ();
  }

let from_preid p =
  {
    id_str = p.Preid.pid_str;
    id_attrs = p.pid_attrs;
    id_loc = p.pid_loc;
    id_local = false;
    id_tag = gen_tag ();
  }

let to_local (id : t) = { id with id_local = true }
let clone (id : t) = { id with id_tag = gen_tag () }
