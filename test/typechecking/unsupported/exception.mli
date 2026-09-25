exception E of [ `A | `B ]

val f : int -> int
(*@ y = f x
    raises E e
      ensures True *)
(* {gospel_expected|
[1] File "./exception.mli", line 5, characters 11-12:
    5 |     raises E e
                   ^
    Error: Not yet supported: E
    
|gospel_expected} *)
