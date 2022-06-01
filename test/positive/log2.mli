val log2 : int -> int
(*@ r = log2 [i: integer] x
    requires i >= 0
    requires x = pow 2 i
    ensures r = i *)

(* Gospel does not infer the type of ghost parameters *)
val log2b : int -> int
(* @ r = log2b [i] x
    requires i >= 0
    requires x = pow 2 i
    ensures r = i *)

val log2_exists : int -> int
(*@ r = log2_exists x
    requires exists i. i >= 0 /\ x = pow 2 i
    ensures forall i. x = pow 2 i -> r = i *)

val log2_existsb : int -> int
(*@ r = log2_existsb x
    requires exists i. i >= 0 /\ x = pow 2 i
    ensures x = pow 2 r *)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
