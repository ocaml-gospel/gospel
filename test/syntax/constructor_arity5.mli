val f : (int * int) option -> unit
(*@ f x
    modifies x
    requires match x with
             | Some (_,_) -> true
             | _ -> false
*)
