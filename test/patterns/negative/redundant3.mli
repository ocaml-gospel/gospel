type t = A | B of t

val f : t -> int
(*@ y = f x
    ensures match x with
    | A | B _ -> false
    | B (B A) -> false
    | _ -> true *)
