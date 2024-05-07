module type S = sig
  type 'a t
  (*@ mutable model view : 'a sequence *)

  val create : unit -> 'a t
  (*@ s = create ()
        ensures s.view = Sequence.empty *)

  val size : 'a t -> int
  (*@ n = size s
        ensures n = Sequence.length s.view *)

  val is_empty : 'a t -> bool
  (*@ b = is_empty s
        ensures b <-> s.view = Sequence.empty *)

  val push : 'a -> 'a t -> unit
  (*@ push x s
        modifies s
        ensures  s.view = Sequence.cons x (old s.view) *)

  val pop : 'a t -> 'a
  (*@ x = pop s
        requires s.view <> Sequence.empty
        modifies s
        ensures  s.view = (old s.view)[1 ..] *)

  val clear : 'a t -> unit
  (*@ clear s
        modifies s
        ensures  s.view = Sequence.empty *)

  val concat : 'a t -> 'a t -> unit
  (*@ concat s1 s2
        modifies s1, s2
        ensures s1.view = (old s1.view ++ old s2.view)
        ensures s2.view = Sequence.empty *)
end
