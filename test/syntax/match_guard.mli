val f : int -> int
(*@ r = f x
  requires 0 = match x with
           | _ when false->true -> 1
*)

(* read as [false -> (true -> 1)] *)
(* There is no simple way to disambiguate between the match-case [->] and
   implies *)

(* {gospel_expected|
   [125] File "match_guard.mli", line 4, characters 35-36:
         4 |            | _ when false->true -> 1
                                                ^
         Error: A formula was expected.
   |gospel_expected} *)
