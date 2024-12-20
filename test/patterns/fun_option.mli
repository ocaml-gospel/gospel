val f : 'a option list -> bool
(*@ b = f os
    ensures Sequence._exists (fun (None | Some _) -> false) os.list_content
*)
