(* we would expect to be able to destruct a tuple in an anonymous function *)

val f : ('a * 'b * 'c) list -> 'a list
(*@ xs = f ys
    ensures xs = List.map (fun (x, _, _) -> x) ys
*)

(* {gospel_expected|
   [125] File "t5.mli", line 5, characters 31-32:
         5 |     ensures xs = List.map (fun (x, _, _) -> x) ys
                                            ^
         Error: Syntax error.
   |gospel_expected} *)
