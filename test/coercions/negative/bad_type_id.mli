type t1

(*@ function c (x: t1) : t1 *)
(*@ coercion *)

(* EXPECTED
   [125] File "bad_type_id.mli", line 3, characters 10-11:
         Error: The function `c' does not have a valid coercion type.
*)
