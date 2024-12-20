val f : int option -> int
(*@ r = f x
    requires match x with
             | Some (y:int) -> y.v >= 0
             | None -> true *)
