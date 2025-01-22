type t
(*@ ephemeral
    model view: int *)

val f : t -> int
(*@ y = f x
    pure
    modifies x *)

(* {gospel_expected|
   [125] File "pure_no_modifies.mli", line 6, characters 8-9:
         6 | (*@ y = f x
                     ^
         Error: Type checking error: a pure function cannot have writes.
   |gospel_expected} *)
