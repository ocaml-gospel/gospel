type t = C of int * int

val f : int -> t -> unit
(*@ f n t
    requires match t with
             | C (_,_,_)
             | _ -> true *)

(* {gospel_expected|
   [125] File "constructor_arity2.mli", line 6, characters 15-24:
         6 |              | C (_,_,_)
                            ^^^^^^^^^
         Error: The constructor C expects 2 argument(s)
                but is applied to 3 argument(s) here.
   |gospel_expected} *)
