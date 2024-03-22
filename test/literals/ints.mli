type t = A of int

val f : int -> int
(*@ y = f x
    requires x = 42i
    requires 42 = 42i
    requires 42i = 42i
    requires 42 = 0x16
    requires A 42i = A 0x16i
    requires A 42i = A 43i *)
