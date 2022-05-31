type t
(*@ mutable model view: int *)

val f : t -> int
(*@ y = f x
    pure
    modifies x *)

(* EXPECTED
   [125] File "impure1.mli", line 5, characters 5-6:
         Error: Type checking error: a pure function cannot have writes.
*)
