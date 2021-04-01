(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2019   --   Inria - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Ppxlib

exception Ocaml_syntax_error of Location.t

val with_loadpath : string list -> string -> string
(** [with_loadpath loadpath filename] finds the first directory [d] in
    [loadpath] such that [d/filename] is a valid file path, and returns it. If
    [filename] is an absolute valid path or is ["gospelstdlib.mli"], it returns
    it unchanged. Raises Not_found if no such path exists. *)

(* val parse_ocaml : string -> Parsetree.signature *)
(** `parse_ocaml file` parses the OCaml content of the `file` if it is a valid
    interface.

    Raise Not_found if file does not exist. Raise Ocaml_syntax_error if there is
    an OCaml syntax error. *)

val parse_ocaml_signature : string -> Parsetree.signature
val parse_ocaml_signature_lb : Lexing.lexbuf -> Parsetree.signature

val parse_ocaml_structure_lb : Lexing.lexbuf -> Parsetree.structure

(* val parse_gospel :
 *   filename:string -> Parsetree.signature -> string -> Uast.s_signature *)
(** [parse_gospel sig_list module_name] parses the GOSPEL attributes and
    integrates them in the corresponding OCaml signatures. *)

(** [parse_ocaml_gospel path] parses the OCaml interface and the GOSPEL
    specification of the file located in [path].

    Raise Not_found if the file does not exist. Raise Ocaml_syntax_error if
    there is an OCaml syntax error. *)

val parse_signature_gospel :
  filename:string -> Parsetree.signature -> string -> Uast.s_signature

val parse_structure_gospel :
  filename:string -> Parsetree.structure -> string -> Uast.s_structure

val parse_ocaml_signature_gospel : string -> Uast.s_signature

(* val parse_ocaml_structure_gospel : string -> Uast.s_structure *)
