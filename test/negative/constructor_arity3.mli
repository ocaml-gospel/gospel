type t = L of t * t | E

val f : t -> int
(*@ r = f a
    ensures
      match a with
      | L E E -> r = 1
*)
