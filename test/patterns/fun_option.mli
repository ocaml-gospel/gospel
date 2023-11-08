val f : 'a option list -> bool
(*@ b = f os
    ensures List._exists (fun (None | Some _) -> false) os
*)
