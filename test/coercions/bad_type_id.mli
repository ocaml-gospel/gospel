type t1

(*@ function c (x: t1) : t1 *)
(*@ coercion *)

(* {gospel_expected|
   [125] File "bad_type_id.mli", line 3, characters 13-14:
         3 | (*@ function c (x: t1) : t1 *)
                          ^
         Error: The function c does not have a valid coercion type.
   |gospel_expected} *)
