type t0 = A | B
type t1 = t0
(*@ model : integer *)
(* {gospel_expected|
[1] File "./type_alias.mli", line 2, characters 10-12:
    2 | type t1 = t0
                  ^^
    Error: Not yet supported: t0
    
|gospel_expected} *)
