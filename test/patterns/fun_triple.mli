val f : ('a * 'b * 'c) list -> 'a list
(*@ xs = f ys
    ensures xs = Sequence.map (fun (x, _, _) : 'a -> x) ys
*)
