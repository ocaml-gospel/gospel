(*@ type 'a memory *)

(*@ predicate extend (m m': 'a memory) *)
(*@ axiom refl: forall m: 'a memory. extend m m *)
(*@ axiom tran: forall m1 m2 m3: 'a memory.
      extend m1 m2 -> extend m2 m3 ->
      extend m1 m3 *)

(*@ val create_mem : unit -> 'a memory *)
module Ephemeral : sig
  type 'a stack
  (*@ ephemeral
      model : 'a memory -> 'a Sequence.t *)

  (*@ axiom stack_mon:
        forall m m': 'a memory, e: 'a stack.
        extend m m' ->
        e m = e m' *)

  val ecreate : 'a -> 'a stack
  (*@ r = ecreate [m: 'a memory] x
        produces r @ 'a stack
        ensures r m = Sequence.empty *)

  val epush : 'a stack -> 'a -> unit
  (*@ epush [m: 'a memory] s x
        modifies s @ 'a stack
        ensures  s m = Sequence.cons x (old s m) *)

  val epop : 'a stack -> 'a
  (*@ r = epop [m: 'a memory] s
        modifies s @ 'a stack
        requires s m <> Sequence.empty
        ensures  (old s m) = Sequence.cons r (s m) *)
end

module Persistent : sig
  type 'a stack
  (*@ ephemeral
      model pview: 'a memory -> 'a Sequence.t *)

  (*@ axiom pstack_mon:
        forall m m': 'a memory, p: 'a stack.
        extend m m' ->
        p.pview m = p.pview m' *)

  val pcreate : 'a -> 'a stack
  (*@ r, [m': 'a memory] = pcreate [m: 'a memory] x
        ensures r.pview m' = Sequence.empty
        ensures extend m m' *)

  val ppush : 'a stack -> 'a -> 'a stack
  (*@ r, [m': 'a memory] = ppush [m: 'a memory] s x
        modifies s
        ensures  r.pview m' = Sequence.cons x (s.pview m)
        ensures  extend m m' *)

  val ppop : 'a stack -> 'a stack * 'a
  (*@ (rs, res) = ppop [m: 'a memory] s
        modifies s
        requires s.pview m <> Sequence.empty
        ensures  s.pview m = Sequence.cons res (rs.pview m) *)
end

val pstack_to_estack : 'a Persistent.stack -> 'a Ephemeral.stack
(*@ re = pstack_to_estack [m: 'a memory] ps
      ensures re m = ps.Persistent.pview m *)

val estack_to_pstack : 'a Ephemeral.stack -> 'a Persistent.stack
(*@ rp, [m': 'a memory] = estack_to_pstack [m: 'a memory] es
      consumes es
      ensures  rp.Persistent.pview m' = (old es) m
      ensures  extend m m' *)
