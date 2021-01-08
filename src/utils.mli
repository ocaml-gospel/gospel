module Option : sig
  val value : 'a option -> default:'a -> 'a
  (** [value o ~default] is [v] if [o] is [Some v] and default otherwise. *)

  val get : 'a option -> 'a
  (** [get o] is [v] if [o] is [Some v]. Raises Invalid_argument otherwise. *)

  val map : ('a -> 'b) -> 'a option -> 'b option
  (** [map f o] is [None] if [o] is [None] and [Some (f v)] is [o] is [Some v]. *)

  val iter : ('a -> unit) -> 'a option -> unit
  (** [iter f o] is [f v] if [o] is [Some] v and [()] otherwise. *)

  val is_some : 'a option -> bool
  (** [is_some o] is [true] iff [o] is [Some o]. *)

  val fold : none:'a -> some:('b -> 'a) -> 'b option -> 'a
  (** [fold ~none ~some o] is [none] if [o] is [None] and [some v] if [o] is
      [Some v]. *)
end

val split_at_f : ('a -> bool) -> 'a list -> 'a list * 'a list
(** [split_at_f f l] is the partition [(l1, l2)] such that [l1] is the longest
    prefix where the predicate [f] holds. The order of the elements is not
    changed. *)

val split_at_i : int -> 'a list -> 'a list * 'a list
(** [split_at_i i l] is the partition [(l1, l2)] such that [l1] contains the
    first i elements. The order of the elements is not changed. *)

module Fmt : sig
  include module type of struct include Fmt end

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
  (** [lparens ppf ()] is [any "@[<1>("] *)

  val rparens : 'a t
  (** [rparens ppf ()] is [any ")@]"] *)

  val lbracket : 'a t
  (** [lbracket ppf ()] is [any "@[<1>["] *)

  val rbracket : 'a t
  (** [rbracket ppf ()] is [any "]@]"] *)

  val lbrace : 'a t
  (** [lbrace ppf ()] is [any "@[<1>{"] *)

  val rbrace : 'a t
  (** [rbrace ppf ()] is [any "}@]"] *)
end

exception TypeCheckingError of string

exception NotSupported of string

exception Located of Location.t * exn

val error : ?loc:Location.t -> exn -> 'a
(** [error ?loc e] raises [e], wrapped in [Located(loc, e)] if [loc] is
    provided. *)

val check : ?loc:Location.t -> bool -> exn -> unit
(** [check ?loc b e] checks if [b] is true, otherwise raises [e], wrapped in
    [Located(loc, e)] if [loc] is provided. *)

val error_report : ?loc:Location.t -> string -> 'a
(** [error_report ?loc e] is [error ?loc (TypeCheckingError s)]. *)

val check_report : ?loc:Location.t -> bool -> string -> unit
(** [check_report ?loc b e] is [check ?loc b (TypeCheckingError s)]. **)

val not_supported : ?loc:Location.t -> string -> 'a
(** [not_supported ?loc s] raises [NotSupported s], wrapped in [Located(loc, e)]
    if [loc] is provided. *)

(** String sets. *)
module Sstr : Set.S with type elt = string
