type t = ..
(*@ model : integer *)
(* {gospel_expected|
[1] File "./type_extension_with_spec.mli", line 1, characters 0-34:
    1 | type t = ..
    2 | (*@ model : integer *)
    Error: Not yet supported: extensible type
    
|gospel_expected} *)
