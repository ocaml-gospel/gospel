val f : int -> int
(*@ r = f x
  requires match x with
           | 1i -> true
           | _ when 1 = 2 -> false
*)
