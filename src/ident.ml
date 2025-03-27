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
module type ID = sig
  type t
  (** The type of unique identifiers. *)

  val equal : t -> t -> bool
  (** Equality over identifiers. *)

  val hash : t -> int
  (** Hash function for identifiers. *)

  val set_project_name : string -> unit
  (** This function should be called exactly once so that the type checker
      generates identifiers that are unique to this project. This means that
      when we use files from this project in other projects, there will be no
      clashes with regards to identifiers as long as both projects are named
      differently. *)

  val gen_id : unit -> t
  (** Generates a fresh identifier. *)
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

  let project_name = ref "##PRIMITIVES##"
  let equal x y = x.id = y.id && x.project = y.project
  let hash x = x.id
  let set_project_name nm = project_name := nm

  let gen_id : unit -> t =
    let r = ref 0 in
    fun () ->
      r := !r + 1;
      { id = !r; project = !project_name }
end

module IdTable = Hashtbl.Make (Tag)

type t = {
  id_str : string; (* Variable name. Not used internally. *)
  id_attrs : string list; (* Variable attributes *)
  id_loc : Location.t;
  id_tag : Tag.t;
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
let equal x y = Tag.equal x.id_tag y.id_tag
let hash x = Tag.hash x.id_tag

let mk_id id_str id_loc =
  { id_str; id_attrs = []; id_loc; id_tag = Tag.gen_id () }

let stdlib_id = mk_id "Gospelstdlib" Location.none

let tvar nm =
  { id_str = nm; id_attrs = []; id_loc = Location.none; id_tag = Tag.gen_id () }

let from_preid p =
  {
    id_str = p.Preid.pid_str;
    id_attrs = p.pid_attrs;
    id_loc = p.pid_loc;
    id_tag = Tag.gen_id ();
  }
