
val f16: int -> int
(*@ r = f16 x
  requires 0 = match x with
           | _ when false->true -> 1 (* read as false -> (true -> 1) *)
*)
