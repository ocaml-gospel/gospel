val scalar_product : int list -> int -> int list

(*@ r = scalar_product v s
    ensures Sequence.map (fun x -> x.v) r.list_content = Sequence.map ((fun x y -> x * y.v) s.v) v.list_content *)
