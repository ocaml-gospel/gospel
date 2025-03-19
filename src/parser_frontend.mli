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

val parse_ocaml : string -> signature
(** `parse_ocaml file` parses the OCaml content of the `file` if it is a valid
    interface.

    Raise Not_found if file does not exist. Raise Syntax_error if there is an
    OCaml syntax error. *)

val parse_gospel : filename:string -> signature -> Parse_uast.s_signature
(** [parse_gospel filename sig_list] parses the GOSPEL attributes and integrates
    them in the corresponding OCaml signatures. *)

val parse_ocaml_gospel : string -> Parse_uast.s_signature
(** [parse_ocaml_gospel path] parses the OCaml interface and the GOSPEL
    specification of the file located in [path].

    Raise Not_found if the file does not exist. Raise Syntax_error if there is
    an OCaml syntax error. *)
