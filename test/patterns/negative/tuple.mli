type t = bool * int

(*@ function f (a : t) : bool =
      match a with
      | true, 0i -> true
      | false, x -> false
*)
