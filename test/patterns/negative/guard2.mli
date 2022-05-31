val f : int -> int
(*@ y = f x
      requires 0 = match x with _ when match 1 with _ -> true -> 1 | _ -> 2 *)

(* missing parentheses around nested match *)
