type ('a, 'b) t
(*@ model m : ('a, 'b) list *)

(* {gospel_expected|
   [125] Error: The type list expects 1 argument(s) but was given 2 argument(s) here.
   |gospel_expected} *)
