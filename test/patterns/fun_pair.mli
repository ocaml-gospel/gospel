val f : (int * 'a) list -> int list
(*@ ys = f xs
    ensures ys = List.map (fun (x, _) -> x) xs
*)
