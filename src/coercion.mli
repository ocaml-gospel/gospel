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

open Ttypes
open Symbols

type t
(** a set of coercions *)

val empty : t

val add : t -> lsymbol -> t
(** adds a new coercion from function [ls: ty1 -> ty2 ] raises an error if

    - this introduces a cycle, i.e. a coercion from [ty2] to [ty1] is already
      defined;
    - function [ls] cannot be used as a coercion, e.g. [ty1 = ty2];
    - a coercion from [ty1] to [ty2] is already defined *)

val find : t -> ty -> ty -> lsymbol list
(** returns the coercion, or raises [Not_found] *)

val union : t -> t -> t
(** [union s1 s2] merges the coercions from [s2] into [s1] (as a new set of
    coercions) this is asymetric: a coercion from [s2] may hide a coercion from
    [s1] *)
