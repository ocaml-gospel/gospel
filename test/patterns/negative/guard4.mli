type t = A

val f : t -> int
(*@ r = f x
  requires match x with
           | A when 1 = 1 -> true
           | _ when true -> false
*)
