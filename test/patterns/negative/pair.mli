type 'a t = P of 'a * int

val f : int t -> int
(*@ r = f a
    ensures
      match a with
      | P(x, 0i) -> true
      | P(1i, x) -> false
*)
