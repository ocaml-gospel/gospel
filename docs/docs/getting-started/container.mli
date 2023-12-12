type 'a t
(** The type for containers. *)
(*@ model capacity: int
    mutable model contents: 'a set
    with t
    invariant t.capacity > 0
    invariant Set.cardinal t.contents <= t.capacity *)

exception Full

val create : int -> 'a t
(** [create capacity] is an empty container whose maximum capacity is
    [capacity]. *)
(*@ t = create c
    requires c > 0
    ensures t.capacity = c
    ensures t.contents = Set.empty *)

val is_empty : 'a t -> bool
(** [is_empty t] is [true] iff [t] contains no elements. *)
(*@ b = is_empty t
    pure
    ensures b <-> t.contents = Set.empty *)

val clear : 'a t -> unit
(** [clear t] removes all values in [t]. *)
(*@ clear t
    modifies t.contents
    ensures is_empty t *)

val add : 'a t -> 'a -> unit
(** [add t x] adds [x] to the container [t], or raises [Full] if [t] has reached
    its maximum capacity. *)
(*@ add t x
    modifies t.contents
    ensures t.contents = Set.add x (old t.contents)
    raises Full -> Set.cardinal (old t.contents) = t.capacity
                /\ t.contents = old t.contents *)

val mem : 'a t -> 'a -> bool
(** [mem t x] is [true] iff [t] contains [x]. *)
(*@ b = mem t x
    pure
    ensures b <-> Set.mem x t.contents *)
