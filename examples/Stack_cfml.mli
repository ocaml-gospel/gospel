module type S = sig
  type 'a t
  (*@ ephemeral
      model : 'a Sequence.t *)

  val create : unit -> 'a t
  (*@ q = create ()
        ensures q = Sequence.empty *)

  val is_empty : 'a t -> bool
  (*@ b = is_empty q
        preserves q @ 'a t
        ensures b <-> q = Sequence.empty *)

  val push : 'a t -> 'a -> unit
  (*@ push p x
        modifies p @ 'a t
        ensures p = Sequence.cons x (old p) *)

  val pop : 'a t -> 'a
  (*@ r = pop p
        modifies p @ 'a t
        raises Not_found
        ensures (old p) = Sequence.cons r p *)

  val clear : 'a t -> unit
  (*@ clear p
        modifies p @ 'a t
        ensures p = Sequence.empty *)

  val concat : 'a t -> 'a t -> unit
  (*@ concat q1 q2
        modifies q1 @ 'a t
        modifies q2 @ 'a t
        ensures q1 = old (q1 ++ q2)
        ensures q2 = Sequence.empty *)

  val rev_append : 'a t -> 'a t -> unit
  (*@ rev_append p1 p2
        modifies p1 @ 'a t
        modifies p2 @ 'a t
        ensures p1 = Sequence.empty
        ensures p2 = old (Sequence.rev p1 ++ p2) *)
end
