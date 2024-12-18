val f : (int * 'a) list -> int list
(*@ ys = f xs
    ensures ys = Sequence.map (fun (x, _) -> x) xs
*)
