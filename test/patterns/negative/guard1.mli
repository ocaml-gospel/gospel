val f : int -> int
(*@ r = f x
  requires 0 = match x with
           | _ when false->true -> 1
*)

(* read as false -> (true -> 1) *)
(* {gospel_expected|
   [125] File "guard1.mli", line 4, characters 35-36:
         4 |            | _ when false->true -> 1
                                                ^
         Error: A formula was expected.
   |gospel_expected} *)
