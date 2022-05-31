val f : int -> int
(*@ r = f x
  requires 0 = match x with
           | _ when false->true -> 1
*)

(* read as false -> (true -> 1) *)
