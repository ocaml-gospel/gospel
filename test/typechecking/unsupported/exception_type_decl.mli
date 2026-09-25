type t = A | B

exception E of t

val f : int -> int
(*@ y = f x
    raises E e
      ensures True *)
(* {gospel_expected|
[1] File "./exception_type_decl.mli", line 7, characters 11-12:
    7 |     raises E e
                   ^
    Error: Not yet supported: E
    
|gospel_expected} *)
