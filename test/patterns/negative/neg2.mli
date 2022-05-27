type 'a t = 'a * int

val f : int t -> int
(*@ r = f a
    ensures
      match a with
      | x, 0i -> true
      | 1i, x -> false
*)
