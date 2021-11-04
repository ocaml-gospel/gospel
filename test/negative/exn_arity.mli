exception E of int * int

val f : int -> unit
(*@ f i
    raises E _ -> false *)
