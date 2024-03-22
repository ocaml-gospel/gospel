(* we would expect a nicer way to specify a function returning unit with a
   ghost value *)

val f : int -> unit
(*@ [y : integer] = f x
    modifies true *)

(* {gospel_expected|
   [125] File "return_unit_and_ghost.mli", line 5, characters 20-21:
         5 | (*@ [y : integer] = f x
                                 ^
         Error: Type checking error: too few returned values.
   |gospel_expected} *)
