(*@ type t = A of { b : bool } *)

(*@ function f (b : bool) : t = A b *)
(* The type-checker is expected to fail here *)
