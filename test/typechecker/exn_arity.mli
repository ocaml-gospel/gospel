exception E of int * int

val f : int -> unit
(*@ f i
    raises E _ -> false *)

(* {gospel_expected|
   [125] File "exn_arity.mli", line 5, characters 11-23:
         5 |     raises E _ -> false *)
                        ^^^^^^^^^^^^
         Error: Type checking error: Exception pattern doesn't match its type.
   |gospel_expected} *)
