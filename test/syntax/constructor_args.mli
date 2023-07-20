type t = C of int * int

val f : int -> int
(*@ m = f n
    requires C (n, n) = C n n
*)

(* {gospel_expected|
   [125] File "constructor_args.mli", line 5, characters 24-25:
         5 |     requires C (n, n) = C n n
                                     ^
         Error: Syntax error.
   |gospel_expected} *)
