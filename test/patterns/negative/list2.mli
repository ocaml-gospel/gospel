val f : 'a list option -> int
(*@ r = f l
    requires match l with
      | None -> false
      | Some (x :: _ as a) -> false
*)

(* EXPECTED
   [125] File "list2.mli", line 3, characters 13-83:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  Some [].
*)
