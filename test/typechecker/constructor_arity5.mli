type t = C of int * int

val f : int -> int
(*@ m = f n
    requires C (n, n, n) = C (n, n)
*)

(* {gospel_expected|
   [125] File "constructor_arity5.mli", line 5, characters 13-14:
         5 |     requires C (n, n, n) = C (n, n)
                          ^
         Error: The constructor C expects 2 argument(s)
                but is applied to 3 argument(s) here.
   |gospel_expected} *)
