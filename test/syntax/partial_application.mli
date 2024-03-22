val scalar_product : int list -> int -> int list

(*@ r = scalar_product v s
    ensures List.map integer_of_int r = List.map ((fun x y -> x * integer_of_int y) (integer_of_int s)) v *)
