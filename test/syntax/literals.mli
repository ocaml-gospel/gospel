(** Tests for constant literals *)

val f : int -> float
(*@ y = f x
    requires x.v = 0
    ensures y = 0. *)

val g : char -> string
(*@ y = g x
    requires x = 'c'
    ensures y = "c" *)
