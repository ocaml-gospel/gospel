(*@ type m = A of { a : bool } *)

(*@ function f (a : m) : bool =
      match a with
       | A { a } -> a
*)
