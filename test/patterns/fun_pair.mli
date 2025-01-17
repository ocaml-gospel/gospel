val f : (int * 'a) list -> int list
(*@ ys = f xs
    ensures ys.list_content = Sequence.map (fun (x, _) -> x) xs.list_content
*)
