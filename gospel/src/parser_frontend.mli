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

exception Ocaml_syntax_error of Location.t

(** [with_loadpath loadpath filename] finds the first directory [d] in
   [loadpath] such that [d/filename] is a valid file path, and returns it. If
   [filename] is an absolute valid path or is ["gospelstdlib.mli"], it returns
   it unchanged. Raises Not_found if no such path exists. *)
val with_loadpath : string list -> string -> string

(** `parse_ocaml file` parses the OCaml content of the `file` if it is a valid
   interface.

   Raise Not_found if file does not exist.
   Raise Ocaml_syntax_error if there is an OCaml syntax error. *)
val parse_ocaml : string -> Oparsetree.signature

val parse_ocaml_lb : Lexing.lexbuf -> Oparsetree.signature

(** [parse_gospel sig_list name] parses the GOSPEL attributes and
   integrates them in the corresponding OCaml signatures. *)
val parse_gospel :
  Oparsetree.signature_item list -> string -> Uast.s_signature_item list

(** [parse_ocaml_gospel file] parses the OCaml interface and
   the GOSPEL specification of `file`.

   Raise Not_found if file does not exist.
   Raise Ocaml_syntax_error if there is an OCaml syntax error. *)
val parse_ocaml_gospel :  string -> string -> Uast.s_signature_item list
