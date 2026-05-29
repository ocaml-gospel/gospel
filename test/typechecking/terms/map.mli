type ('a, 'b) t
(*@ mutable model : ('a, 'b) map *)

val add : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
(*@ m1 = add m0 a b
    ensures m1 = m0[a -> b]
*)
(* {gospel_expected|
[1] File "map.mli", line 6, characters 20-21:
    6 |     ensures m1 = m0[a -> b]
                            ^
    Error: Mismatch between type 'a and type 'a
    
|gospel_expected} *)
