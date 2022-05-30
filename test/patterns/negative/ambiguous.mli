val f : int -> int
(*@ y = f x
    ensures match x with
    | _ | _ when x = 1 -> true
    | _ -> false *)
