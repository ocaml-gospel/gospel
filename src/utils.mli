(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val duplicate : ('a -> 'a -> bool) -> ('a -> unit) -> 'a list -> unit
(** [has_duplicates p error l] calls [error x], where [x] is an element of [l],
    if there exists some [y] that also belongs to [l] and is not physically
    equal to [x] where [p x y] holds. This function always call [error] with the
    value that occurs later in the list. Has no visible effect if no two
    elements in the list satisfy [p]. This function has quadratic time
    complexity and therefore should only be used on very small lists.*)

exception Cycle of string * string list
(** [raise (Cycle s l)] returns the start (and, naturally end) of a cycle [s]
    and the list of nodes [l] that make up the rest of the cycle. If [l] is
    empty, then there is an edge that connects [s] to itself. *)

val depends : (string * string list) list -> string list
(** [depends eq error graph] returns a list [l] with each node in [graph] where,
    for each [i, j] where [0 <= i <= j < List.length l], the node [List.nth l j]
    cannot be reached from [List.nth l i] through a non-empty path, according to
    [graph].

    @raise Cycle When [graph] has a cycle. *)

module Fmt : sig
  include module type of struct
    include Fmt
  end

  val list : ?first:unit t -> ?last:unit t -> ?sep:unit t -> 'a t -> 'a list t

  val pp : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  (** [pp] is [pf] *)

  (** {1 More separators} *)

  val full : 'a t
  (** [full ppf ()] is [any ".@ "] *)

  val arrow : 'a t
  (** [arrow ppf ()] is [any " ->@ "] *)

  val star : 'a t
  (** [star ppf ()] is [any " *@ "] *)

  val newline : 'a t
  (** [newline ppf ()] is [any "@\n"] *)

  (** {1 More brackets} *)

  val lparens : 'a t
  (** [lparens ppf ()] is [any "@\[<1>("] *)

  val rparens : 'a t
  (** [rparens ppf ()] is [any ")@\]"] *)

  val lbracket : 'a t
  (** [lbracket ppf ()] is [any "@\[<1>\["] *)

  val rbracket : 'a t
  (** [rbracket ppf ()] is [any "\]@\]"] *)

  val lbrace : 'a t
  (** [lbrace ppf ()] is [any "@\[<1>{"] *)

  val rbrace : 'a t
  (** [rbrace ppf ()] is [any "}@\]"] *)

  val pp_loc : Ppxlib.Location.t Fmt.t
end

module Sstr : Set.S with type elt = string
(** String sets. *)
