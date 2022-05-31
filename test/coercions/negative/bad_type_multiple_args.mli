type t1
type t2

(*@ function c (x: t1) (y: t1) : t2 *)
(*@ coercion *)

(* EXPECTED
   [125] File "bad_type_multiple_args.mli", line 4, characters 10-11:
         Error: The function `c' does not have a valid coercion type.
*)
