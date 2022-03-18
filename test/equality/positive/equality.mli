val f : int -> int -> bool
(*@ equality *)

type t

val g : t -> t -> bool
(*@ equality *)

type 'a u

val h : 'a u -> 'a u -> bool
(*@ equality *)

type v = t

val i : v -> t -> bool
(*@ equality *)
