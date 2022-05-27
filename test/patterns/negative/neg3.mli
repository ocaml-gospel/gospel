type t7 = char * char

val f7 : t7 -> int
(*@ r = f7 a
    ensures
      match a with
      | '\000', x -> true
      | x, '\000' -> true
*)
