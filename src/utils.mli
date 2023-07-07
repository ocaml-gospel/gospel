val split_at_f : ('a -> bool) -> 'a list -> 'a list * 'a list
(** [split_at_f f l] is the partition [(l1, l2)] such that [l1] is the longest
    prefix where the predicate [f] holds. The order of the elements is not
    changed. *)

val split_at_i : int -> 'a list -> 'a list * 'a list
(** [split_at_i i l] is the partition [(l1, l2)] such that [l1] contains the
    first i elements. The order of the elements is not changed. *)

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
