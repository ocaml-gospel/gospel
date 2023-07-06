type t = C of int * int

val f : int -> int
(*@ m = f n
    requires C (n, n, n) = C (n, n)
*)

(* {gospel_expected|
   [125] File "constructor_arity5.mli", line 5, characters 13-14:
         5 |     requires C (n, n, n) = C (n, n)
                          ^
         Error: The symbol `C' cannot be partially applied.
   |gospel_expected} *)
