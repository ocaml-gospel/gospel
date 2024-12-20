val f : ('a * 'b * 'c) list -> 'a list
(*@ xs = f ys
    ensures xs.list_content = Sequence.map (fun (x, _, _) : 'a -> x) ys.list_content
*)
