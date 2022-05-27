(*
type t = {
  a : int;
  b : t * char;
  c : string
}
val f : t -> int
(*@ r = f x
    ensures match x with
    | {a=1i} -> true
    | {a=_;b=_, '\000'..'z';c="coucou"} -> true
    | {b=_, 'z'..'\255'} -> true
 *) *)

type t2 = P of int * (t2 * char) * string
(*@ function f (x: t2) : bool =
      match x with
      | P(1i, _, _) -> true
      | P(_,(_, '\000'..'z'),"coucou") -> true
      | P(_,(_, 'z'..'\255'),_) -> true *)

(* {a=0; b=(_, '\000'); c=""} *)
