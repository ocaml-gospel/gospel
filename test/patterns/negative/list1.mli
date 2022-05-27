val f : 'a list -> int
(*@ r = f l
    requires match l with
      | [] -> true
      | x::y::_ -> false
*)
