(*@ function logand (x : integer) (y : integer) : integer *)

type t = private { size : int; mutable mask : int }
(*@ with self
    invariant 0 <= self.size.v <= 63
    invariant 0 <= self.mask.v < pow 2 self.size.v *)

(*@ predicate mem (i: integer) (bv: t) = logand bv.mask.v (pow 2 i) <> 0 *)

val create : int -> t
(*@ bv = create n
    checks 0 <= n.v <= 63
    ensures bv.size.v = n.v
    ensures forall i. 0 <= i < n.v -> not (mem i bv) *)

val add : int -> t -> unit
(*@ add i bv
    checks 0 <= i.v < bv.size.v
    modifies bv
    ensures forall j. 0 <= j < bv.size.v ->
              mem j bv <-> i.v = j \/ mem j (old bv) *)

val mem : int -> t -> bool
(*@ b = mem i bv
    checks 0 <= i.v < bv.size.v
    ensures b <-> mem i.v bv *)
