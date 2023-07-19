val f : ('a * 'b * 'c) list -> 'a list
(*@ xs = f ys
    ensures xs = List.map (fun (x, _, _) -> x) ys
*)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
