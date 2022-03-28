type t = A of int * int

val f : t -> int
(*@ r = f a
    ensures
      match a with
      | A (1i, 1i) -> true
*)
