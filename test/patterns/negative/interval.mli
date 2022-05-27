type t12 = P of char * int

(*@ function f (a : t12) : bool =
      match a with
      | P ('\000'..'b', 0i) -> true
      | P ('b'..'\255', x) -> false
*)
