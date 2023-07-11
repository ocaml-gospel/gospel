val f : int -> int
(*@ y = f x
    ensures match x with _ | a -> a = 1
*)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
