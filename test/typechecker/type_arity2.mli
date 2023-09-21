type 'a t

val create : int -> t

(* {gospel_expected|
   [125] File "type_arity2.mli", line 3, characters 20-21:
         3 | val create : int -> t
                                 ^
         Error: The type t expects 1 argument(s) but was given 0 argument(s) here.
   |gospel_expected} *)
