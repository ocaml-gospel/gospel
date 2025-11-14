val f : int -> int option
(*@ o = f i *)
(* {gospel_expected|
[1] File "return_option.mli", line 1, characters 19-25:
    1 | val f : int -> int option
                           ^^^^^^
    Error: Unbound type constructor option
    
|gospel_expected} *)
