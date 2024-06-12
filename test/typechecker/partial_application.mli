val mems : 'a -> 'a list list -> bool
(*@ r = mems x xss

    (* This is accepted *)
    ensures r = List._exists (fun xs -> List.mem x xs) xss

    (* This is not; should it be? *)
    ensures r = List._exists (List.mem x) xss
*)
