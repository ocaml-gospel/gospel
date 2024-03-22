val f : 'a list option -> int
(*@ r = f l
    requires match l with
      | None -> false
      | Some (x :: _ as a) -> false
*)

(* {gospel_expected|
   [125] File "nonexhaustive_inner_list.mli", line 3, characters 13-83:
         3 | .............match l with
         4 |       | None -> false
         5 |       | Some (x :: _ as a) -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  Some [].
   |gospel_expected} *)
