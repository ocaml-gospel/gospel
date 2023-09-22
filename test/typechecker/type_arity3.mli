type 'a t

val create : int -> (char, int) t

(* {gospel_expected|
   [125] File "type_arity3.mli", line 3, characters 20-33:
         3 | val create : int -> (char, int) t
                                 ^^^^^^^^^^^^^
         Error: The type t expects 1 argument(s) but was given 2 argument(s) here.
   |gospel_expected} *)
