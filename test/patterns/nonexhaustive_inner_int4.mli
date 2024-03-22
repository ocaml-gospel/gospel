(*@ type ti = N of integer *)
(*@ function f2 (x : ti) : unit =
      match x with
      | N 0
      | N 1
      | N 3 -> () *)

(* {gospel_expected|
   [125] File "nonexhaustive_inner_int4.mli", line 3, characters 6-60:
         3 | ......match x with
         4 |       | N 0
         5 |       | N 1
         6 |       | N 3 -> ()...
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  N 2.
   |gospel_expected} *)
