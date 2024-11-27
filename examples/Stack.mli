module type S = sig
  type 'a t
  (*@ mutable model view : 'a sequence *)

  val create : unit -> 'a t
  (*@ s = create ()
        produces s @ 'a t
        ensures s.view = Sequence.empty *)

  val size : 'a t -> int
  (*@ n = size s
        preserves s @ 'a t
        ensures n = Sequence.length s.view *)

  val is_empty : 'a t -> bool
  (*@ b = is_empty s
        preserves s @ 'a t
        ensures b <-> s.view = Sequence.empty *)

  val push : 'a -> 'a t -> unit
  (*@ push x s
        modifies s @ 'a t
        ensures  s.view = Sequence.cons x (old s.view) *)

  val pop : 'a t -> 'a
  (*@ x = pop s
        requires s.view <> Sequence.empty
        modifies s @ 'a t
        ensures  s.view = (old s.view)[1 ..] *)

  val clear : 'a t -> unit
  (*@ clear s
        modifies s @ 'a t
        ensures  s.view = Sequence.empty *)

  val concat : 'a t -> 'a t -> unit
  (*@ concat s1 s2
        modifies s1 @ 'a t, s2 @ 'a t
        ensures s1.view = (old s1.view ++ old s2.view)
        ensures s2.view = Sequence.empty *)
end
