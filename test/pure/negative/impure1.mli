
type t
(*@ mutable model view: int *)

val f: t ->  int
(*@ y = f x
    pure
    modifies x *)

