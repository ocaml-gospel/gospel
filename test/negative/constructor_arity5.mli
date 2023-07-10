val f : (int * int) option -> unit
(*@ f x
    modifies x
    requires match x with
             | Some (_,_) -> true
             | _ -> false
*)

(* {gospel_expected|
   [125] File "constructor_arity5.mli", line 4, characters 15-25:
         4 |              | Some (_,_) -> true
                            ^^^^^^^^^^
         Error: The constructor `Some' expects `1' argument(s)
                but is applied to 2 argument(s) here.
   |gospel_expected} *)
