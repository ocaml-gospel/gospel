(*@ type t = A of integer *)

val f : int -> int
(*@ y = f x
    requires A 42i = A 42i *)
