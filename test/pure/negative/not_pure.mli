
val f: int ->  int
(*@ y = f x *)

val g: int -> int
(*@ y = g x
    requires f x > 0 *)

