type t = A | B

exception E of t

val f : int -> int
(*@ y = f x
    raises E e
      ensures True *)
(* {gospel_expected|
[1] File "./exception_type_decl.mli", line 3, characters 15-16:
    3 | exception E of t
                       ^
    Error: Unbound type constructor t
    
|gospel_expected} *)
