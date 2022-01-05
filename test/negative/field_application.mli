type t = { a : int }

val f : t -> int
(*@ r = f x
      ensures r = a x *)
