type t = bool * int

(*@ function f (a : t) : bool =
      match a with
      | true, 0i -> true
      | false, x -> false
*)

(* {gospel_expected|
   [125] File "tuple.mli", line 4, characters 6-69:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  True, 1i.
   |gospel_expected} *)
