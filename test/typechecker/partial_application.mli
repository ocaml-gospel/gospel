val mems : 'a -> 'a list list -> bool
(*@ r = mems x xss

    (* This is accepted *)
    ensures r = List._exists (fun xs -> List.mem x xs) xss

    (* This is not; should it be? *)
    ensures r = List._exists (List.mem x) xss
*)

(* {gospel_expected|
   [125] File "partial_application.mli", line 8, characters 29-41:
         8 |     ensures r = List._exists (List.mem x) xss
                                          ^^^^^^^^^^^^
         Error: Not a function symbol: mem.
   |gospel_expected} *)
