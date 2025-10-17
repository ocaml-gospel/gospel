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

(** Signature for a module of unique identifiers where new values can only be
    generated via the [gen_id] function. *)

let primitive_project = "##PRIMITIVES##"
let stdlib_project = "##STDLIB##"

module type ID = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val set_project_name : string -> unit
  val is_project : t -> string -> bool
  val gen_id : unit -> t
end

(** This implementation of the [Id] defines identifiers as a record consisting
    of an integer and the name of the project. The name of the project is
    necessary because when we unmarshal compiled gospel files it is impossible
    to guarantee that there will not be collisions with integers that we
    generate for other files. *)
module Tag : ID = struct
  type t = { id : int; project : string }
  (** Invariant : two identifiers belong to the same [project] , they cannot
      have the same integer identifier. *)

  let project_name = ref primitive_project
  let equal x y = x.id = y.id && x.project = y.project
  let hash x = x.id
  let set_project_name nm = project_name := nm
  let is_project id nm = id.project = nm

  let gen_id : unit -> t =
    let r = ref 0 in
    fun () ->
      r := !r + 1;
      { id = !r; project = !project_name }
end

module IdTable = Hashtbl.Make (Tag)

type t = {
  id_str : string;
  id_fixity : Preid.fixity;
  id_attrs : string list;
  id_loc : Location.t;
  id_tag : Tag.t;
}

let pp ppf id =
  match id.id_fixity with
  | Normal -> Format.fprintf ppf "%s%a" id.id_str Preid.pp_attrs id.id_attrs
  | _ -> Format.fprintf ppf "(%s)%a" id.id_str Preid.pp_attrs id.id_attrs

let to_string id = id.id_str
let equal x y = Tag.equal x.id_tag y.id_tag
let hash x = Tag.hash x.id_tag

let mk_id ?loc:(id_loc = Location.none) id_str =
  {
    id_str;
    id_fixity = Preid.Normal;
    id_attrs = [];
    id_loc;
    id_tag = Tag.gen_id ();
  }

let stdlib_id = mk_id "Gospelstdlib"
let is_stdlib id = Tag.is_project id.id_tag stdlib_project
let is_primitive id = Tag.is_project id.id_tag primitive_project

let from_preid p =
  {
    id_str = p.Preid.pid_str;
    id_fixity = p.pid_fixity;
    id_attrs = p.pid_attrs;
    id_loc = p.pid_loc;
    id_tag = Tag.gen_id ();
  }
